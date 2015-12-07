map_get_density <- function(
                        properties      = list( provinces , types ),
                        grd_params      = list( nx, ny ),
                        method          = 'splancs2d',
                        dens_fun_params = list( h0 ))
{

  # Mongo Connections
  db <- 'meteor'
  con <- rmongodb::mongo.create(host='localhost:3001',db=db)
        
  # Collections
  grid_collection <- paste(db,'.map_grids',sep="")
  density_collection <- paste(db,'.map_density',sep="")
  provinces_collection <- paste(db,'.map_provinces',sep="")
  traffic_history_collection <- paste(db,'.incidents_history',sep="")

  # Check if the grid exits
  grid_query <- list( properties = properties ,grd_params = grd_params)
  grid_count <- rmongodb::mongo.count(con, grid_collection,query = grid_query )



  if (grid_count == 0){

    provinces_convex_huld <- map_get_provinces_convex_huld(con,provinces_collection,properties$provinces)
    
    map_make_grid ( con , provinces_convex_huld, grid_collection, properties, grd_params )

  } else  if (grid_count >  1){
    print("There are multiple grids created with this characteristics")
    print("FIX ME!")
    return(1)
  }

  density_query <- list( properties = properties, grd_params = grd_params, method = method, dens_fun_params = dens_fun_params )
  density_count <- rmongodb::mongo.count(con, density_collection, query = density_query )

  if (density_count == 0){

    if(!"provinces_convex_huld" %in% ls()) provinces_convex_huld <- map_get_provinces_convex_huld(con,provinces_collection,properties$provinces)

    grd_cursor <- rmongodb::mongo.find(con, grid_collection,query = grid_query )
    grd <- mongo_cursor_to_grid_topology(grd_cursor)

    density_list <-list()
    density_list$properties  <- properties
    density_list$grd_params  <- grd_params
    density_list$method  <- method
    density_list$dens_fun_params <- dens_fun_params
    density_list$values <- mongo_calculate_incicents_density(con, traffic_history_collection, properties$types,provinces_convex_huld,grd,method,dens_fun_params)

    rmongodb::mongo.insert(con,density_collection,density_list)

  } else if (density_count > 1) {

    print("There are multiple density lists with this characteristics")
    print("FIX ME!")
    return(1)

  }
  return(0)

}

mongo_calculate_incicents_density <- function(con,traffic_history_collection,types,provinces_convex_huld,grd,method,dens_fun_params = list( h0 )){

  traffic_history_cursor <- rmongodb::mongo.find(
      con, 
      traffic_history_collection,
      query=list(
                latitude=list('$nin'=c('','0.0')),
                type=ifelse(length(types)==1,types,list('$in' = types))
                ),
      fields=list('_id'=0L,longitude=1,latitude=1,type=1,date=1)
  )

  # This is a dirty fix for the latlon error, and is not the correct way to do
  # it. FIXME.

  traffic_history <- rmongodb::mongo.cursor.to.list(traffic_history_cursor)

  latlon_correction <- function(incident){
    if (incident$latitud < incident$longitud) {
      tmp <- incident$longitud
      incident$longitud <- incident$latitud
      incident$latitud  <- tmp
    }
    return(incident) 
  }
    
  traffic_history <- lapply(traffic_history,latlon_correction)


  # Get the observation points
  obs_points <- cbind(
    x=as.numeric(unlist(lapply(traffic_history,`[[`,'longitude'))),
    y=as.numeric(unlist(lapply(traffic_history,`[[`,'latitude')))
  )

  # Set the coordinate reference system (Mercator) 
  OSM_CRS <- sp::CRS("+init=epsg:3857")
  obs_points <- sp::SpatialPoints(obs_points, proj4string=sp::CRS("+init=epsg:4326"))
  obs_points <- sp::spTransform(obs_points, OSM_CRS)

  # Calculate the cells dimensions and centroids

  if (method == 'splancs2d'){
    map_grid_density_values <- get_density_splancs2d(obs_points,provinces_convex_huld,grd,dens_fun_params, grid_polys)
  } else {
    print(paste(method, "is not a valid method"))
  }

  return(map_grid_density_values)
}


get_density_splancs2d <- function(obs_points,
                                  provinces_convex_huld,
                                  grd,
                                  dens_fun_params = list( h0 ),
                                  grid_polys){
  
  
  # Add properties to the polygons
  
  grid_density_values <- c(splancs::spkernel2d(
                                            as.matrix(as.data.frame(obs_points)),
                                            as.matrix(as.data.frame(provinces_convex_huld)),
                                            h0=as.numeric(dens_fun_params$h0),grd))

  return(grid_density_values)
}
