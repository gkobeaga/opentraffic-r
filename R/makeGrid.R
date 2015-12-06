mongo_cursor_to_grid_topology <- function(cursor,ps4 ='+init=epsg:3857'){  
  cursor <- grd_cursor

  tmp_list <- rmongodb::mongo.cursor.to.list(cursor)

  tmp_array <- rjson::toJSON(tmp_list)
  tmp_json <- substr(tmp_array,2,nchar(tmp_array)-1)

  tmp_file <- paste(tempfile(),'.topojson',sep="")
  write(tmp_json, tmp_file)

  tmp_sp <- geojsonio::topojson_read(tmp_file)
  tmp_topology <- sp::points2grid(sp::SpatialPoints(sp::coordinates(tmp_sp), proj4string = sp::CRS('+init=epsg:3857')),tolerance=0.1)
  points <- sp::SpatialPoints(sp::coordinates(tmp_sp))
  tmp_topology <- sp::points2grid(sp::SpatialPoints(sp::coordinates(tmp_sp)),tolerance=0.1)
  return(tmp_topology)
}

map_get_provinces_convex_huld <- function(con,collection, provinces_selected){
  provinces_query <- paste(
                         '{ 
                            "objects.provinces.geometries.properties.name" : 
                              {"$in" : 
                                  ["',paste(provinces_selected,collapse="\",\""),'"]
                              }
                          }'
                        ,sep="")

  provinces_cursor <- rmongodb::mongo.find(con, collection)#,

  provinces_list <- rmongodb::mongo.cursor.to.list(provinces_cursor)

  tmpfile <- tempfile()
  writeLines(rjson::toJSON(provinces_list[[1]],method='C'),tmpfile)

  provinces_list <- geojsonio::geojson_list(geojsonio::topojson_read('prueba.topojson'))

  provinces_unlist_points <- c()

  for (province in provinces_list$features)
    provinces_unlist_points <- c(provinces_unlist_points, unlist(province$geometry$coordinates))

  provinces_pts <- structure(list(
                  x = provinces_unlist_points[seq(1,length(provinces_unlist_points),2)], 
                  y = provinces_unlist_points[seq(2,length(provinces_unlist_points),2)]),
                  .Names = c("x", "y")
                )

  convex_huld <- lapply(provinces_pts,"[",chull(provinces_pts))
  
  return(convex_huld)

}

map_make_grid <- function(con, provinces_convex_huld, grid_collection, properties, grd_params){
  
  provinces_border <- as.matrix(as.data.frame(provinces_convex_huld))
  
  # Making the grid

  # Grid bbox
  bb <- sp::bbox(provinces_border)

  # Grid cell dimensions
  cd <- c(grd_params$nx,grd_params$ny)
  cs <- as.numeric(diff(t(bb))/cd)
  cc <- as.numeric(bb[, 1] + (cs/2))  # cell offset

  # Construct the grid
  grid_topology <- sp::GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)

  # Set the coordinate reference system (Mercator) 
    
  OSM_CRS <- sp::CRS("+init=epsg:3857")
  # Transform the grid to polygons 
  grid_polys <- sp::as.SpatialPolygons.GridTopology(grid_topology,proj4string=OSM_CRS)

  # Add null data to the polygons 
  polys_df <- data.frame(value=matrix(0,nrow=grd_params$nx*grd_params$ny),row.names=row.names(grid_polys))
  polys_df$id <- row.names(grid_polys)
  grid_polys_df = sp::SpatialPolygonsDataFrame(grid_polys, polys_df)

  tmpfile <- tempfile()
  shpfile <- paste(tmpfile,'.shp',sep='')
  topojsonfile <- paste(tmpfile,'.topojson',sep='')
  rgdal::writeOGR(grid_polys_df, shpfile, layer="grid", driver="ESRI Shapefile")

  system2('topojson', c('-o', topojsonfile, '--id-property id', '-p value=value' ,paste('grid=',shpfile,sep="")))
  grid_list <- rjson::fromJSON(paste(readLines(topojsonfile),collapse=""))

  grid_list$properties <- properties
  grid_list$grd_params <- grd_params

  rmongodb::mongo.insert( con, grid_collection,grid_list)

}
