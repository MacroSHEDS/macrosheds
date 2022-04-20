#' Delineate a watershed from a stream location.
#' 
#' From any geographic location, this function (iteratively) attempts to delineate a watershed,
#' i.e. the land area that contributes overland flow to that location. Delineation
#' can be guided by user knowledge of local geography, or it can be fully agnostic.
#' Any specifications not explicitly passed are given reasonable defaults. Candidate
#' delineations are made available for viewing and the user is asked to pick one,
#' or try again by interactively providing different specifications. See details.
#' 
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Spencer Rhea
#' @author Wes Slaughter
#' @param lat numeric. Represents the latitude of the pour point in decimal degrees
#' (negative indicates southern hemisphere).
#' @param long numeric. Represents the longitude of the pour point in decimal degrees
#' (negative indicates west of prime meridian).
#' @param crs numeric. Represents the coordinate reference system of your starting
#' coordinates. EPSG codes work here (e.g. the default is 4326 for WGS 84). 
#' @param write_dir character string. The directory in which to write output shapefile.
#' @param write_name character string. the basename of the shapefile components to
#' be written. e.g. \code{write_name = 'foo'} would produce foo.shp, foo.shx, foo.prj, foo.dbf.
all the spec_ params
#' @param confirm logical. Ignored unless all delineation parameters
#' (the ones that start with "spec_") are supplied.
#' If TRUE, you will be asked to visually confirm the delineation before it is
#' written to \code{write_dir}.
#' @param verbose logical. Determines the amount of informative messaging during run.
#' @return 
#' A list containing the following components:
#' + watershed_area_ha: the area of the delineated watershed in hectares
#'      (meters squared divided by 10,000)
#' + buffer_radius_m: the width (meters) around the site location that was used when
#'      requesting a DEM (digital elevation model)
#' + snap_distance_m: the search radius (meters) around the pour point that was used
#'      to choose a stream to snap the pour point to.
#' + snap_method: either "standard", which snaps the pour point to the cell
#'      within snap_distance_m with highest flow accumulation, or "jenson",
#'      which snaps to the nearest flow line
#' + dem_resolution: passed to elevatr::get_elev_raster (z parameter).
#'      depends on supplied machine_status
#' + flat_increment: see whitebox::wbt_breach_depressions or
#'      whitebox::wbt_breach_depressions_least_cost
#' + breach_method: string. Either 'basic', indicating that
#'      whitebox::wbt_breach_depressions was used, or 'lc', indicating
#'      whitebox::wbt_breach_depressions_least_cost (which less readily
#'      alters the DEM)
#' + burn_streams: TRUE or FALSE, indicating whether
#'      whitebox::wbt_burn_streams_at_roads and whitebox::wbt_fill_burn were
#'      used on the DEM
#' @details
#' Output files are unprojected (WGS 84), though processing is done
#' on projected data. A projection is chosen automatically by [macrosheds:::choose_projection()],
#' based on pour point location. Note that this has nothing to do with the crs parameter,
#' which only allows you to specify the coordinate reference system of your input
#' coordinates. Also note that for watersheds that span several latitudes or longitudes,
#' calculated watershed areas might be inaccurate.
#' 
#' For the fully agnostic delineation procedure, here are the steps:
#' 1. A reasonable projection is chosen via [choose_projection()].
#' 2. A digital elevation model is retrieved via [elevatr::get_elev_raster()].
#'    This initial DEM is 4 km^2 and is centered on the given location.
#' 3. Single-cell pits in the DEM are filled using [whitebox::wbt_fill_single_cell_pits()].
#'    and larger depressions are breached using [whitebox::wbt_breach_depressions()].
#'    These preprocessing steps ensure that the DEM is traversable by the delineator.
#'    If you'd like to see a "fill depressions" option, please open a
#'    \href{https://github.com/MacroSHEDS/macrosheds/issues}{GitHub issue}.
#' 4. If specified, streamline and roadway files are retrieved via \pkg{osmdata},
#'    and streamlines are \emph{burned} into the DEM at stream-road intersections.
#'    In most cases, this allows delineation to proceed through bridges and culverts
#'    where it would otherwise stop.
#' 5. Flow direction and accumulation are computed via [whitebox::wbt_d8_pointer()]
#'    and [whitebox::wbt_d8_flow_accumulation()].
#' 6. The specified site location is \emph{snapped} to a nearby stream line in
#'    multiple ways, in the hopes that at least one snap will land on the
#'    intended stream. This matters when e.g. a tributary is being delineated
#'    near its confluence with a mainstem. Snapping methods include
#'    [whitebox::wbt_snap_pour_points()] and [whitebox::wbt_jenson_snap_pour_points()].
#' 7. All unique snap locations are delineated via [whitebox::wbt_watershed()]. If
#'    at any point the computed watershed meets the edge of the DEM, a larger DEM
#'    is retrieved, and the delineator tries again.
#' 8. All candidate watersheds are presented for inspection. Either one is selected,
#'    or the procedure starts over with a new set of user-given and/or default
#'    specifications.
#' 9. The delineated watershed is saved as a shapefile. Watershed area
#'    and the successful set of specifications are returned.
#' @export
#' @seealso [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @examples
#' area_and_specs <- ms_delineate_watershed(
#'     lat = 44.21013,
#'     long = -122.2571,
#'     crs = 4326,
#'     write_dir = '/some/path',
#'     write_name = 'example_site'
#' )

spec_buffer_radius_m = 1000
spec_snap_distance_m = 150
spec_snap_method = 'standard'
spec_dem_resolution = 10
spec_flat_increment = NULL
spec_breach_method = 'lc'
spec_burn_streams = FALSE

ms_delineate_watershed <- function(lat,
                                   long,
                                   crs = 4326,
                                   write_dir,
                                   write_name,
                                   verbose = TRUE,
                                   spec_buffer_radius_m = NULL,
                                   spec_snap_distance_m = NULL,
                                   spec_snap_method = NULL,
                                   spec_dem_resolution = NULL,
                                   spec_flat_increment = NULL,
                                   spec_breach_method = NULL,
                                   spec_burn_streams = NULL,
                                   confirm = TRUE){
    
    sm <- suppressMessages
    sw <- suppressWarnings
    library(tidyverse)
    library(glue)
    library(sf)
    library(data.table)
    library(terra)
    library(mapview)
    library(whitebox)
    library(elevatR)
    
    #moving shapefiles can be annoying, since they're actually represented by
    #   3-4 files
    move_shapefiles <- function(shp_files,
                                from_dir,
                                to_dir,
                                new_name_vec = NULL){
        
        #shp_files is a character vector of filenames with .shp extension
        #   (.shx, .prj, .dbf are handled internally and don't need to be listed)
        #from_dir and to_dir are strings representing the source and destination
        #   directories, respectively
        #new_name_vec is an optional character vector of new names for each shape file.
        #   these can end in ".shp", but don't need to
        
        if(any(! grepl('\\.shp$', shp_files))){
            stop('All components of shp_files must end in ".shp"')
        }
        
        if(length(shp_files) != length(new_name_vec)){
            stop('new_name_vec must have the same length as shp_files')
        }
        
        dir.create(to_dir,
                   showWarnings = FALSE,
                   recursive = TRUE)
        
        for(i in 1:length(shp_files)){
            
            shapefile_base <- strsplit(shp_files[i], '\\.shp')[[1]]
            
            files_to_move <- list.files(path = from_dir,
                                        pattern = shapefile_base)
            
            extensions <- str_match(files_to_move,
                                    paste0(shapefile_base, '(\\.[a-z]{3})'))[, 2]
            
            if(is.null(new_name_vec)){
                new_name_base <- rep(shapefile_base, length(files_to_move))
            } else {
                new_name_base <- strsplit(new_name_vec[i], '\\.shp$')[[1]]
                new_name_base <- rep(new_name_base, length(files_to_move))
            }
            
            tryCatch({
                
                #try to move the files (may fail if they are on different partitions)
                mapply(function(x, nm, ext) file.rename(from = paste(from_dir,
                                                                     x,
                                                                     sep = '/'),
                                                        to = glue('{td}/{n}{ex}',
                                                                  td = to_dir,
                                                                  n = nm,
                                                                  ex = ext)),
                       x = files_to_move,
                       nm = new_name_base,
                       ext = extensions)
                
            }, warning = function(w){
                
                #if that fails, copy them and then delete them
                mapply(function(x, nm, ext) file.copy(from = paste(from_dir,
                                                                   x,
                                                                   sep = '/'),
                                                      to = glue('{td}/{n}{ex}',
                                                                td = to_dir,
                                                                n = nm,
                                                                ex = ext),
                                                      overwrite = TRUE),
                       x = files_to_move,
                       nm = new_name_base,
                       ext = extensions)
                
                lapply(paste(from_dir,
                             files_to_move,
                             sep = '/'),
                       unlink)
            })
        }
        
        #return()
    }
    
    #prompt users for stuff, provide single-character responses,
    #   reprompt if they don't choose one of the expected responses
    get_response_1char <- function(msg,
                                   possible_chars,
                                   subsequent_prompt = FALSE){
        
        #msg: character. a message that will be used to prompt the user
        #possible_chars: character vector of acceptable single-character responses
        #subsequent prompt: not to be set directly. This is handled by
        #   get_response_1char during recursion.
        
        if(subsequent_prompt){
            cat(paste('Please choose one of:',
                      paste(possible_chars,
                            collapse = ', '),
                      '\n> '))
        } else {
            cat(msg)
        }
        
        ch <- as.character(readLines(con = stdin(), 1))
        
        if(length(ch) == 1 && ch %in% possible_chars){
            return(ch)
        } else {
            get_response_1char(msg = msg,
                               possible_chars = possible_chars,
                               subsequent_prompt = TRUE)
        }
    }
    
    #prompt users for stuff, provide multi-character responses,
    #   reprompt if they don't choose one of the expected responses
    get_response_mchar <- function(msg,
                                   possible_resps,
                                   allow_alphanumeric_response = TRUE,
                                   subsequent_prompt = FALSE){
        
        #msg: character. a message that will be used to prompt the user
        #possible_resps: character vector. If length 1, each character in the response
        #   will be required to match a character in possible_resps, and the return
        #   value will be a character vector of each single-character tokens in the
        #   response. If
        #   length > 1, the response will be required to match an element of
        #   possible_resps exactly, and the response will be returned as-is.
        #allow_alphanumeric_response: logical. If FALSE, the response may not
        #   include both numerals and letters. Only applies when possible_resps
        #   has length 1.
        #subsequent prompt: not to be set directly. This is handled by
        #   get_response_mchar during recursion.
        
        split_by_character <- ifelse(length(possible_resps) == 1, TRUE, FALSE)
        
        if(subsequent_prompt){
            
            if(split_by_character){
                pr <- strsplit(possible_resps, split = '')[[1]]
            } else {
                pr <- possible_resps
            }
            
            cat(paste('Your options are:',
                      paste(pr,
                            collapse = ', '),
                      '\n> '))
        } else {
            cat(msg)
        }
        
        chs <- as.character(readLines(con = stdin(), 1))
        
        if(! allow_alphanumeric_response &&
           split_by_character &&
           grepl('[0-9]', chs) &&
           grepl('[a-zA-Z]', chs)){
            
            cat('Response may not include both letters and numbers.\n> ')
            resp <- get_response_mchar(
                msg = msg,
                possible_resps = possible_resps,
                allow_alphanumeric_response = allow_alphanumeric_response,
                subsequent_prompt = FALSE)
            
            return(resp)
        }
        
        if(length(chs)){
            if(split_by_character){
                
                if(length(possible_resps) != 1){
                    stop('possible_resps must be length 1 if split_by_character is TRUE')
                }
                
                chs <- strsplit(chs, split = '')[[1]]
                possible_resps_split <- strsplit(possible_resps, split = '')[[1]]
                
                if(all(chs %in% possible_resps_split)){
                    return(chs)
                }
                
            } else {
                
                if(length(possible_resps) < 2){
                    stop('possible_resps must have length > 1 if split_by_character is FALSE')
                }
                
                if(any(possible_resps == chs)){
                    return(chs)
                }
            }
        }
        
        resp <- get_response_mchar(
            msg = msg,
            possible_resps = possible_resps,
            allow_alphanumeric_response = allow_alphanumeric_response,
            subsequent_prompt = TRUE)
        
        return(resp)
    }
    
    #chooose an appropriate projection, based on location
    choose_projection <- function(lat = NULL,
                                  long = NULL,
                                  unprojected = FALSE){
        
        #TODO: CHOOSE PROJECTIONS MORE CAREFULLY
        
        if(unprojected){
            PROJ4 <- glue('+proj=longlat +datum=WGS84 +no_defs ',
                          '+ellps=WGS84 +towgs84=0,0,0')
            return(PROJ4)
        }
        
        if(is.null(lat) || is.null(long)){
            stop('If projecting, lat and long are required.')
        }
        
        abslat <- abs(lat)
        
        # if(abslat < 23){ #tropical
        #     PROJ4 = glue('+proj=laea +lon_0=', long)
        #              # ' +datum=WGS84 +units=m +no_defs')
        # } else { #temperate or polar
        #     PROJ4 = glue('+proj=laea +lat_0=', lat, ' +lon_0=', long)
        # }
        
        #this is what the makers of https://projectionwizard.org/# use to choose
        #a suitable projection: https://rdrr.io/cran/rCAT/man/simProjWiz.html
        # THIS WORKS (PROJECTS STUFF), BUT CAN'T BE READ AUTOMATICALLY BY st_read
        if(abslat < 70){ #tropical or temperate
            PROJ4 <- glue('+proj=cea +lon_0={lng} +lat_ts=0 +x_0=0 +y_0=0 ',
                          '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                          lng = long)
        } else { #polar
            PROJ4 <- glue('+proj=laea +lat_0={lt} +lon_0={lng} +x_0=0 +y_0=0 ',
                          '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                          lt = lat,
                          lng = long)
        }
        
        ## UTM/UPS would be nice for watersheds that don't fall on more than two zones
        ## (incomplete)
        # if(lat > 84 || lat < -80){ #polar; use Universal Polar Stereographic (UPS)
        #     PROJ4 <- glue('+proj=ups +lon_0=', long)
        #              # ' +datum=WGS84 +units=m +no_defs')
        # } else { #not polar; use UTM
        #     PROJ4 <- glue('+proj=utm +lat_0=', lat, ' +lon_0=', long)
        # }
        
        ## EXTRA CODE FOR CHOOSING PROJECTION BY LATITUDE ONLY
        # if(abslat < 23){ #tropical
        #     PROJ4 <- 9835 #Lambert cylindrical equal area (ellipsoidal; should spherical 9834 be used instead?)
        # } else if(abslat > 23 && abslat < 66){ # middle latitudes
        #     PROJ4 <- 5070 #albers equal area conic
        # } else { #polar (abslat >= 66)
        #     PROJ4 <- 9820 #lambert equal area azimuthal
        #     # PROJ4 <- 1027 #lambert equal area azimuthal (spherical)
        # }
        # PROJ4 <- 3857 #WGS 84 / Pseudo-Mercator
        # PROJ4 <- 2163
        
        return(PROJ4)
    }
    
    #choose appropriate granularity of the elevation model,
    #   based on the approximate size of the task (area potentially covered)
    choose_dem_resolution <- function(dev_machine_status, buffer_radius){
        
        if(dev_machine_status == '1337'){
            dem_resolution <- case_when(
                buffer_radius <= 1e4 ~ 12,
                buffer_radius == 1e5 ~ 11,
                buffer_radius == 1e6 ~ 10,
                buffer_radius == 1e7 ~ 8,
                buffer_radius == 1e8 ~ 6,
                buffer_radius == 1e9 ~ 4,
                buffer_radius >= 1e10 ~ 2)
        } else if(dev_machine_status == 'n00b'){
            dem_resolution <- case_when(
                buffer_radius <= 1e4 ~ 10,
                buffer_radius == 1e5 ~ 8,
                buffer_radius == 1e6 ~ 6,
                buffer_radius == 1e7 ~ 4,
                buffer_radius == 1e8 ~ 2,
                buffer_radius >= 1e9 ~ 1)
        } else {
            stop('dev_machine_status must be either "1337" or "n00b"')
        }
        
        return(dem_resolution)
    }
    
    #for determining whether the DEM extent wasn't big enough to allow full
    #   delineation
    raster_intersection_summary <- function(wb, dem){
        
        #wb is a delineated watershed boundary as a rasterLayer
        #dem is a DEM rasterLayer
        
        summary_out <- list()
        
        #convert wb to sf object (there are several benign but seemingly uncatchable
        #   garbage collection errors here)
        wb <- sf::st_as_sf(terra::as.polygons(wb))
        
        #get edge of DEM as sf object
        # dem_edge <- dem %>%
        #     terra::rast() %>%
        #     terra::boundaries() %>%
        #     terra::as.polygons() %>%
        #     sf::st_as_sf() %>%
        #     sf::st_boundary()
        
        # dem_edge <- dem %>%
        #     raster::focal(., #the terra version doesn't retain NA border
        #                  fun=function(x) return(0), na.rm=F,
        #                  w = matrix(1, nrow = 3, ncol = 3)) %>%
        #     raster::reclassify(rcl = matrix(c(0, NA,
        #                                       NA, 0), #set inner cells to NA
        #                                     ncol = 2)) %>%
        #     raster::rasterToPolygons() %>%
        #     sf::st_as_sf()
        
        get_out_cells <- function(x) {
            w <- sum(x, na.rm = FALSE)
            if(is.na(w)){
                return(0)
            } else{
                return(NA)
            }
        }
        
        dem_edge <- dem %>%
            terra::rast() %>%
            terra::focal(., fun=get_out_cells,
                         w = matrix(1, nrow = 3, ncol = 3)) %>%
            terra::as.polygons(dissolve = FALSE) %>%
            sf::st_as_sf() %>%
            # dem_edge was lossing it's crs or it was changing
            sf::st_transform(sf::st_crs(wb))
        
        #tally raster cells
        summary_out$n_wb_cells <- length(wb$geometry)
        summary_out$n_dem_cells <- length(dem_edge$geometry)
        
        #tally intersections; calc percent of wb cells that overlap
        intersections <- sf::st_intersects(wb, dem_edge) %>%
            as.matrix() %>%
            apply(MARGIN = 2,
                  FUN = sum) %>%
            table()
        
        true_intersections <- sum(intersections[names(intersections) > 0])
        
        summary_out$n_intersections <- true_intersections
        summary_out$pct_wb_cells_intersect <- true_intersections /
            summary_out$n_wb_cells * 100
        
        return(summary_out)
    }
    
    #handles ephemeral download errors
    expo_backoff <- function(expr,
                             max_attempts = 5,
                             verbose = TRUE){
        
        for(attempt_i in seq_len(max_attempts)){
            
            results <- try(expr = expr,
                           silent = TRUE)
            
            if(inherits(results, 'try-error')){
                
                if(attempt_i == max_attempts){
                    stop(attr(results, 'condition'))
                }
                
                backoff <- runif(n = 1,
                                 min = 0,
                                 max = 2^attempt_i - 1)
                
                if(verbose){
                    print(glue("Backing off for ", round(backoff, 1), " seconds."))
                }
                
                Sys.sleep(backoff)
                
            } else {
                
                # if(verbose){
                #     print(paste0("Request succeeded after ", attempt_i, " attempt(s)."))
                # }
                
                break
            }
        }
        
        return(results)
    }
    
    #for retrieving openStreetMap layers (roads)
    get_osm_roads <- function(extent_raster, outfile = NULL){
        
        #extent_raster: either a terra spatRaster or a rasterLayer. The output
        #   roads will have the same crs, and roughly the same extent, as this raster.
        #outfile: string. If supplied, output shapefile will be written to this
        #   location. If not supplied, the output will be returned.
        
        message('Downloading roads layer from OpenStreetMap')
        
        extent_raster <- terra::rast(extent_raster)
        # rast_crs <- as.character(extent_raster@crs)
        rast_crs <- terra::crs(extent_raster,
                               proj = TRUE)
        
        extent_raster_wgs84 <- terra::project(extent_raster,
                                              y = 'epsg:4326')
        
        dem_bounds <- terra::ext(extent_raster_wgs84)[c(1, 3, 2, 4)]
        
        highway_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')
        highway_types <- c(highway_types,
                           paste(highway_types, 'link', sep = '_'))
        
        roads_query <- osmdata::opq(dem_bounds) %>%
            osmdata::add_osm_feature(key = 'highway',
                                     value = highway_types)
        
        roads_query$prefix <- sub('timeout:25', 'timeout:180', roads_query$prefix)
        
        roads <- osmdata::osmdata_sf(roads_query)
        roads <- roads$osm_lines$geometry
        
        # plot(roads$osm_lines, max.plot = 1)
        
        roads_proj <- roads %>%
            sf::st_transform(crs = rast_crs) %>%
            sf::st_union() %>%
            # sf::st_transform(crs = WGS84) %>%
            sf::st_as_sf() %>%
            rename(geometry = x) %>%
            mutate(FID = 0:(n() - 1)) %>%
            dplyr::select(FID, geometry)
        
        if(! is.null(outfile)){
            
            sf::st_write(roads_proj,
                         dsn = outfile,
                         layer = 'roads',
                         driver = 'ESRI Shapefile',
                         delete_layer = TRUE,
                         quiet = TRUE)
            
            message(paste('OSM roads layer written to', outfile))
            
        } else {
            return(roads_proj)
        }
    }
    
    #for retrieving openStreetMap layers (streams)
    get_osm_streams <- function(extent_raster, outfile = NULL){
        
        #extent_raster: either a terra spatRaster or a rasterLayer. The output
        #   streams will have the same crs, and roughly the same extent, as this raster.
        #outfile: string. If supplied, output shapefile will be written to this
        #   location. If not supplied, the output will be returned.
        
        message('Downloading streams layer from OpenStreetMap')
        
        extent_raster <- terra::rast(extent_raster)
        # rast_crs <- as.character(extent_raster@crs)
        rast_crs <- terra::crs(extent_raster,
                               proj = TRUE)
        
        extent_raster_wgs84 <- terra::project(extent_raster,
                                              y = 'epsg:4326')
        
        dem_bounds <- terra::ext(extent_raster_wgs84)[c(1, 3, 2, 4)]
        
        streams_query <- osmdata::opq(dem_bounds) %>%
            osmdata::add_osm_feature(key = 'waterway',
                                     value = c('river', 'stream'))
        
        streams_query$prefix <- sub('timeout:25', 'timeout:180', streams_query$prefix)
        
        streams <- osmdata::osmdata_sf(streams_query)
        streams <- streams$osm_lines$geometry
        
        streams_proj <- streams %>%
            sf::st_transform(crs = rast_crs) %>%
            sf::st_union() %>%
            # sf::st_transform(crs = WGS84) %>%
            sf::st_as_sf() %>%
            rename(geometry = x) %>%
            mutate(FID = 0:(n() - 1)) %>%
            dplyr::select(FID, geometry)
        
        if(! is.null(outfile)){
            
            sf::st_write(streams_proj,
                         dsn = outfile,
                         layer = 'streams',
                         driver = 'ESRI Shapefile',
                         delete_layer = TRUE,
                         quiet = TRUE)
            
            message(paste('OSM streams layer written to', outfile))
            
        } else {
            return(streams_proj)
        }
    }
    
    fill_sf_holes <- function(x){
        
        #x: an sf object (probably needs to be projected)
        
        #if there are spaces in a shapefile polygon that are not filled in,
        #   this fills them.
        
        #if the first element of an sf geometry (which is a list) contains multiple
        #   elements, every element after the first is a hole. the first element
        #   is the outer geometry. so replace the geometry with a new polygon that
        #   is only the outer geometry
        
        wb_geom <- sf::st_geometry(x)
        # wb_geom_crs <- sf::st_crs(wb_geom)
        
        n_polygons <- length(wb_geom[[1]])
        if(n_polygons > 1){
            wb_geom[[1]] <- sf::st_polygon(wb_geom[[1]][1])
        }
        
        # if(length(wb_geom) != 1){
        #     wb_geom <- sf::st_combine(wb_geom)
        # }
        
        sf::st_geometry(x) <- wb_geom
        
        return(x)
    }
    
    #the function that runs the workhorse
    delineate_watershed_apriori_recurse <- function(lat,
                                                    long,
                                                    crs,
                                                    site_code,
                                                    buffer_radius = NULL,
                                                    snap_dist = NULL,
                                                    snap_method = NULL,
                                                    dem_resolution = NULL,
                                                    flat_increment = NULL,
                                                    breach_method = 'lc',
                                                    burn_streams = FALSE,
                                                    confirm = TRUE,
                                                    scratch_dir = tempdir(),
                                                    write_dir,
                                                    dev_machine_status = 'n00b',
                                                    verbose = FALSE){
        
        #This function calls delineate_watershed_apriori recursively, taking
        #   user input after each call, until the user selects a delineation
        #   or aborts. For parameter documentation, see delineate_watershed_apriori.
        
        # tmp <- tempdir()
        scratch_dir <- stringr::str_replace_all(scratch_dir, '\\\\', '/')
        
        delin_out <- delineate_watershed_apriori(
            lat = lat,
            long = long,
            crs = crs,
            site_code = site_code,
            snap_dist = snap_dist,
            snap_method = snap_method,
            dem_resolution = dem_resolution,
            flat_increment = flat_increment,
            breach_method = breach_method,
            burn_streams = burn_streams,
            buffer_radius = buffer_radius,
            scratch_dir = scratch_dir,
            dev_machine_status = dev_machine_status,
            verbose = verbose)
        
        inspection_dir <- delin_out$inspection_dir
        
        files_to_inspect <- list.files(path = inspection_dir,
                                       pattern = '.shp')
        
        if(! confirm){
            
            move_shapefiles(shp_files = files_to_inspect[1],
                            from_dir = inspection_dir,
                            to_dir = write_dir,
                            new_name_vec = site_code)
            
            message(glue('Delineation successful and confirm == FALSE. Shapefile written to ',
                         write_dir))
            
            return(files_to_inspect[1])
        }
        
        temp_point <- glue(scratch_dir, '/', 'POINT')
        
        tibble(longitude = long, latitude = lat) %>%
            sf::st_as_sf(coords = c('longitude', 'latitude'),
                         crs = crs) %>%
            sf::st_write(dsn = temp_point,
                         driver = 'ESRI Shapefile',
                         delete_dsn = TRUE,
                         quiet = TRUE)
        
        nshapes <- length(files_to_inspect)
        numeric_selections <- paste('Accept delineation', 1:nshapes)
        
        wb_selections <- paste(paste0('[',
                                      c(1:nshapes, 'M', 'D', 'S', 'B', 'U', 'R', 'I', 'n', 'a'),
                                      ']'),
                               c(numeric_selections,
                                 'Select pourpoint snapping method',
                                 'Set pourpoint snapping maximum distance (meters)',
                                 'Burn streams into the DEM (may help delineator across road-stream intersections)',
                                 'Use more aggressive breaching method (temporary default, pending whitebox bugfix)',
                                 'Set buffer radius (distance from pourpoint to include in DEM download; meters)',
                                 'Select DEM resolution',
                                 'Set flat_increment',
                                 'Next (skip this one for now)',
                                 'Abort delineation'),
                               sep = ': ',
                               collapse = '\n')
        
        helper_code <- glue('{id}.\nmapview::mapviewOptions(fgb = FALSE);',
                            'mapview::mapview(sf::st_read("{wd}/{f}")) + ',
                            'mapview::mapview(sf::st_read("{pf}"))',
                            id = 1:length(files_to_inspect),
                            wd = inspection_dir,
                            f = files_to_inspect,
                            pf = temp_point) %>%
            paste(collapse = '\n\n')
        
        msg <- glue('Visually inspect the watershed boundary candidate shapefiles ',
                    'by pasting the mapview lines below into a separate instance of R.\n\n{hc}\n\n',
                    'Enter the number corresponding to the ',
                    'one that looks most legit, or select one or more tuning ',
                    'options (e.g. "SBRI" without quotes). You usually won\'t ',
                    'need to tune anything. If you aren\'t ',
                    'sure which delineation is correct, get a site manager to verify:\n',
                    'request_site_manager_verification(type=\'wb delin\', ',
                    'network, domain) [function not yet built]\n\nChoices:\n{sel}\n\nEnter choice(s) here > ',
                    hc = helper_code,
                    sel = wb_selections)
        
        resp <- get_response_mchar(
            msg = msg,
            possible_resps = paste(c(1:nshapes, 'M', 'D', 'S', 'B', 'U', 'R', 'I', 'n', 'a'),
                                   collapse = ''),
            allow_alphanumeric_response = FALSE)
        
        if('n' %in% resp){
            unlink(write_dir,
                   recursive = TRUE)
            print(glue('Moving on. You haven\'t seen the last of {s}!',
                       s = site_code))
            return(1)
        }
        
        if('a' %in% resp){
            unlink(write_dir,
                   recursive = TRUE)
            print(glue('Aborted. Any completed delineations have been saved.'))
            return(2)
        }
        
        if('M' %in% resp){
            snap_method <- get_response_1char(
                msg = paste0('Standard snapping moves the pourpoint to the cell with ',
                             'the highest flow accumulation within snap_dist. Jenson ',
                             'method tries to snap to the nearest stream cell.\n\n',
                             '1. Jenson\n2. Standard\n\n',
                             'Enter choice here > '),
                possible_chars = paste(1:2))
            snap_method <- ifelse(snap_method == '1', 'jenson', 'standard')
        }
        
        if('D' %in% resp){
            snap_dist <- get_response_int(
                msg = paste0('Enter a snap distance between 0 and 200 (meters) > '),
                min_val = 0,
                max_val = 200)
        }
        
        if('S' %in% resp){
            burn_streams <- TRUE
        } else {
            burn_streams <- FALSE
        }
        
        if('B' %in% resp){
            breach_method <- 'basic'
        } else {
            breach_method <- 'basic' #TODO: undo this when whitebox is fixed
            # breach_method <- 'lc'
        }
        
        if('U' %in% resp){
            buffer_radius <- get_response_int(
                msg = paste0('Enter a buffer radius between 1000 and 100000 (meters) > '),
                min_val = 0,
                max_val = 100000)
        }
        
        if('R' %in% resp){
            dem_resolution <- get_response_mchar(
                msg = paste0('Choose DEM resolution between 1 (low) and 14 (high)',
                             ' to pass to elevatr::get_elev_raster. For tiny ',
                             'watersheds, use 12-13. For giant ones, use 8-9.\n\n',
                             'Enter choice here > '),
                possible_resps = paste(1:14))
            dem_resolution <- as.numeric(dem_resolution)
        }
        
        if('I' %in% resp){
            
            bm <- ifelse(breach_method == 'basic',
                         'whitebox::wbt_breach_depressions',
                         'whitebox::wbt_breach_depressions_least_cost')
            
            new_options <- paste(paste0('[',
                                        c('S', 'M', 'L'),
                                        ']'),
                                 c('0.001', '0.01', '0.1'),
                                 sep = ': ',
                                 collapse = '\n')
            
            resp2 <- get_response_1char(
                msg = glue('Pick the size of the elevation increment to pass to ',
                           bm, '.\n\n', new_options, '\n\nEnter choice here > '),
                possible_chars = c('S', 'M', 'L'))
            
            flat_increment <- switch(resp2,
                                     S = 0.001,
                                     M = 0.01,
                                     L = 0.1)
        }
        
        if(! grepl('[0-9]', resp)){
            
            if(is.null(buffer_radius)){
                buffer_radius_ <- delin_out$buffer_radius
            } else {
                buffer_radius_ <- buffer_radius
            }
            
            selection <- delineate_watershed_apriori_recurse(
                lat = lat,
                long = long,
                crs = crs,
                site_code = site_code,
                snap_dist = snap_dist,
                snap_method = snap_method,
                dem_resolution = dem_resolution,
                flat_increment = flat_increment,
                breach_method = breach_method,
                burn_streams = burn_streams,
                buffer_radius = buffer_radius_,
                scratch_dir = scratch_dir,
                write_dir = write_dir,
                dev_machine_status = dev_machine_status,
                verbose = verbose)
            
            return(selection)
        }
        
        selection <- files_to_inspect[as.numeric(resp)]
        
        move_shapefiles(shp_files = selection,
                        from_dir = inspection_dir,
                        to_dir = write_dir,
                        new_name_vec = site_code)
        
        message(glue('Selection {s}:\n\t{sel}\nwas written to:\n\t{sdr}',
                     s = resp,
                     sel = selection,
                     sdr = write_dir))
        
        return(selection)
    }
    
    #the workhorse
    delineate_watershed_apriori <- function(lat,
                                            long,
                                            crs,
                                            site_code,
                                            snap_dist = NULL,
                                            snap_method = NULL,
                                            dem_resolution = NULL,
                                            flat_increment = NULL,
                                            breach_method = 'basic',
                                            burn_streams = FALSE,
                                            buffer_radius = NULL,
                                            scratch_dir = tempdir(),
                                            dev_machine_status = 'n00b',
                                            verbose = FALSE){
        
        #lat: numeric representing latitude in decimal degrees
        #   (negative indicates southern hemisphere)
        #long: numeric representing longitude in decimal degrees
        #   (negative indicates west of prime meridian)
        #crs: numeric representing the coordinate reference system (e.g. WSG84)
        #buffer_radius: integer. the width (m) of the buffer around the site location.
        #   a DEM will be acquired that covers at least the full area of the buffer.
        #snap_dist: integer. the distance (m) around the recorded site location
        #   to search for a flow path.
        #snap_method: character. either "standard", which snaps the site location
        #   to the cell within snap_dist that has the highest flow value, or
        #   "jenson", which snaps to the nearest flow path, regardless of flow.
        #dem_resolution: optional integer 1-14. the granularity of the DEM that is used for
        #   delineation. this argument is passed directly to the z parameter of
        #   elevatr::get_elev_raster. 1 is low resolution; 14 is high. If NULL,
        #   this is determined automatically.
        #flat_increment: float or NULL. Passed to
        #   whitebox::wbt_breach_depressions_least_cost
        #   or whitebox::wbt_breach_depressions, depending on the value
        #   of breach_method (see next).
        #breach_method: string. Either 'basic', which invokes whitebox::wbt_breach_depressions,
        #   or 'lc', which invokes whitebox::wbt_breach_depressions_least_cost
        #burn_streams: logical. if TRUE, both whitebox::wbt_burn_streams_at_roads
        #   and whitebox::wbt_fill_burn are called on the DEM, using road and stream
        #   layers from OpenStreetMap.
        #scratch_dir: the directory where intermediate files will be dumped. This
        #   is a randomly generated temporary directory if not specified.
        #dev_machine_status: either '1337', indicating that your machine has >= 16 GB
        #   RAM, or 'n00b', indicating < 16 GB RAM. DEM resolution is chosen accordingly
        #verbose: logical. determines the amount of informative messaging during run
        
        #returns the location of candidate watershed boundary files
        
        # tmp <- tempdir()
        # tmp <- str_replace_all(tmp, '\\\\', '/')
        
        if(! is.null(dem_resolution) && ! is.numeric(dem_resolution)){
            stop('dem_resolution must be a numeric integer or NULL')
        }
        if(! is.null(flat_increment) && ! is.numeric(flat_increment)){
            stop('flat_increment must be numeric or NULL')
        }
        if(! is.null(snap_dist) && ! is.numeric(snap_dist)){
            stop('snap_dist must be numeric or NULL')
        }
        if(! is.null(buffer_radius) && ! is.numeric(buffer_radius)){
            stop('buffer_radius must be numeric or NULL')
        }
        if(! is.null(snap_method) && ! snap_method %in% c('jenson', 'standard')){
            stop('snap_dist must be "jenson", "standard", or NULL')
        }
        if(! breach_method %in% c('lc', 'basic')) stop('breach_method must be "basic" or "lc"')
        if(! is.logical(burn_streams)) stop('burn_streams must be logical')
        
        inspection_dir <- glue(scratch_dir, '/INSPECT_THESE')
        point_dir <- glue(scratch_dir, '/POINT')
        dem_f <- glue(scratch_dir, '/dem.tif')
        point_f <- glue(scratch_dir, '/point.shp')
        streams_f <- glue(scratch_dir, '/streams.shp')
        roads_f <- glue(scratch_dir, '/roads.shp')
        d8_f <- glue(scratch_dir, '/d8_pntr.tif')
        flow_f <- glue(scratch_dir, '/flow.tif')
        
        dir.create(path = inspection_dir,
                   showWarnings = FALSE)
        
        old_files <- list.files(inspection_dir)
        
        if(length(old_files) > 0){
            file.remove(file.path(inspection_dir, old_files))
        }
        
        proj <- choose_projection(lat = lat,
                                  long = long)
        
        site <- tibble(x = lat,
                       y = long) %>%
            sf::st_as_sf(coords = c("y", "x"),
                         crs = crs) %>%
            sf::st_transform(proj)
        # sf::st_transform(4326) #WGS 84 (would be nice to do this unprojected)
        
        #prepare for delineation loops
        if(is.null(buffer_radius)) buffer_radius <- 1000
        dem_coverage_insufficient <- FALSE
        while_loop_begin <- TRUE
        
        #snap site to flowlines 3 different ways. delineate watershed boundaries (wb)
        #for each unique snap. if the delineations get cut off, get more elevation data
        #and try again
        while(while_loop_begin || dem_coverage_insufficient){
            
            while_loop_begin <- FALSE
            
            if(is.null(dem_resolution)){
                # dem_resolution <- choose_dem_resolution(
                #     dev_machine_status = dev_machine_status,
                #     buffer_radius = buffer_radius)
                dem_resolution <- 10
            }
            
            if(verbose){
                
                if(is.null(flat_increment)){
                    fi <- 'auto'
                } else {
                    fi <- as.character(flat_increment)
                }
                
                if(breach_method == 'lc') breach_method <- 'lc (jk, temporarily "basic")'
                print(glue('Delineation specs for this attempt:\n',
                           '\tsite_code: {st}; ',
                           'dem_resolution: {dr}; flat_increment: {fi}\n',
                           '\tbreach_method: {bm}; burn_streams: {bs}\n',
                           '\tbuffer_radius: {br}; snap_method: {smt}\n',
                           '\tsnap_dist: {sdt}',
                           st = site_code,
                           dr = dem_resolution,
                           fi = fi,
                           bm = breach_method,
                           bs = as.character(burn_streams),
                           br = buffer_radius,
                           smt = ifelse(is.null(snap_method),
                                        'auto',
                                        as.character(snap_method)),
                           sdt = ifelse(is.null(snap_dist),
                                        'auto',
                                        as.character(snap_dist)),
                           .trim = FALSE))
            }
            
            site_buf <- sf::st_buffer(x = site,
                                      dist = buffer_radius)
            
            dem <- expo_backoff(
                expr = {
                    elevatr::get_elev_raster(locations = site_buf,
                                             z = dem_resolution,
                                             verbose = FALSE,
                                             override_size_check = TRUE)
                },
                max_attempts = 5
            )
            
            # terra::writeRaster(x = dem,
            terra::writeRaster(x = dem,
                               filename = dem_f,
                               overwrite = TRUE)
            
            #loses projection?
            sf::st_write(obj = site,
                         dsn = point_f,
                         delete_layer = TRUE,
                         quiet = TRUE)
            
            if(burn_streams){
                get_osm_roads(extent_raster = dem,
                              outfile = roads_f)
                get_osm_streams(extent_raster = dem,
                                outfile = streams_f)
            }
            
            whitebox::wbt_fill_single_cell_pits(dem = dem_f,
                                                output = dem_f) %>% invisible()
            
            if(breach_method == 'basic'){
                
                whitebox::wbt_breach_depressions(
                    dem = dem_f,
                    output = dem_f,
                    flat_increment = flat_increment) %>% invisible()
                
            } else if(breach_method == 'lc'){
                
                whitebox::wbt_breach_depressions_least_cost(
                    dem = dem_f,
                    output = dem_f,
                    dist = 10000, #maximum trench length
                    fill = TRUE,
                    flat_increment = flat_increment) %>% invisible()
            }
            #also see wbt_fill_depressions for when there are open pit mines
            
            if(burn_streams){
                
                #the secret is that BOTH of these burns can work in tandem!
                whitebox::wbt_burn_streams_at_roads(dem = dem_f,
                                                    streams = streams_f,
                                                    roads = roads_f,
                                                    output = dem_f,
                                                    width = 50) %>% invisible()
                whitebox::wbt_fill_burn(dem = dem_f,
                                        streams = streams_f,
                                        output = dem_f) %>% invisible()
            }
            
            whitebox::wbt_d8_pointer(dem = dem_f,
                                     output = d8_f) %>% invisible()
            
            whitebox::wbt_d8_flow_accumulation(input = dem_f,
                                               output = flow_f,
                                               out_type = 'catchment area') %>% invisible()
            
            if(! is.null(snap_method)){
                snap_method_func <- ifelse(snap_method == 'standard',
                                           whitebox::wbt_snap_pour_points,
                                           whitebox::wbt_jenson_snap_pour_points)
            }
            
            if(is.null(snap_dist) && is.null(snap_method)){
                
                snap1_f <- glue(scratch_dir, '/snap1_jenson_dist150.shp')
                whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                      streams = flow_f,
                                                      output = snap1_f,
                                                      snap_dist = 150) %>% invisible()
                snap2_f <- glue(scratch_dir, '/snap2_standard_dist50.shp')
                whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                               flow_accum = flow_f,
                                               output = snap2_f,
                                               snap_dist = 50) %>% invisible()
                snap3_f <- glue(scratch_dir, '/snap3_standard_dist150.shp')
                whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                               flow_accum = flow_f,
                                               output = snap3_f,
                                               snap_dist = 150) %>% invisible()
                
                #the site has been snapped 3 different ways. identify unique snap locations.
                snap1 <- sf::st_read(snap1_f, quiet = TRUE)
                snap2 <- sf::st_read(snap2_f, quiet = TRUE)
                snap3 <- sf::st_read(snap3_f, quiet = TRUE)
                unique_snaps_f <- snap1_f
                if(! identical(snap1, snap2)) unique_snaps_f <- c(unique_snaps_f, snap2_f)
                if(! identical(snap1, snap3)) unique_snaps_f <- c(unique_snaps_f, snap3_f)
                
            } else if(is.null(snap_dist)){
                
                snap1_f <- glue('{scrd}/snap1_{smet}_dist150.shp',
                                scrd = scratch_dir,
                                smet = snap_method)
                
                snap2_f <- glue('{scrd}/snap2_{smet}_dist50.shp',
                                scrd = scratch_dir,
                                smet = snap_method)
                
                if(snap_method == 'standard'){
                    
                    whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                                   flow_accum = flow_f,
                                                   output = snap1_f,
                                                   snap_dist = 150)
                    whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                                   flow_accum = flow_f,
                                                   output = snap2_f,
                                                   snap_dist = 50)
                } else {
                    
                    whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                          streams = flow_f,
                                                          output = snap1_f,
                                                          snap_dist = 150)
                    whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                          streams = flow_f,
                                                          output = snap1_f,
                                                          snap_dist = 50)
                }
                
                #the site has been snapped 2 different ways. identify unique snap locations.
                snap1 <- sf::st_read(snap1_f, quiet = TRUE)
                snap2 <- sf::st_read(snap2_f, quiet = TRUE)
                unique_snaps_f <- snap1_f
                if(! identical(snap1, snap2)) unique_snaps_f <- c(unique_snaps_f, snap2_f)
                
            } else if(is.null(snap_method)){
                
                snap1_f <- glue('{scrd}/snap1_jenson_dist{sdst}.shp',
                                scrd = scratch_dir,
                                sdst = snap_dist)
                
                whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                      streams = flow_f,
                                                      output = snap1_f,
                                                      snap_dist = snap_dist) %>% invisible()
                
                snap2_f <- glue('{scrd}/snap2_standard_dist{sdst}.shp',
                                scrd = scratch_dir,
                                sdst = snap_dist)
                
                whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                               flow_accum = flow_f,
                                               output = snap2_f,
                                               snap_dist = snap_dist) %>% invisible()
                
                #the site has been snapped 2 different ways. identify unique snap locations.
                snap1 <- sf::st_read(snap1_f, quiet = TRUE)
                snap2 <- sf::st_read(snap2_f, quiet = TRUE)
                unique_snaps_f <- snap1_f
                if(! identical(snap1, snap2)) unique_snaps_f <- c(unique_snaps_f, snap2_f)
                
            } else {
                
                snap1_f <- glue('{scrd}/snap1_{smet}_dist{sdst}.shp',
                                scrd = scratch_dir,
                                smet = snap_method,
                                sdst = snap_dist)
                
                snap_arglist <- list(pour_pts = point_f,
                                     output = snap1_f,
                                     snap_dist = snap_dist)
                
                if(snap_method == 'standard'){
                    whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                                   flow_accum = flow_f,
                                                   output = snap1_f,
                                                   snap_dist = snap_dist)
                    # snap_arglist$flow_accum = flow_f
                } else {
                    whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                          streams = flow_f,
                                                          output = snap1_f,
                                                          snap_dist = snap_dist)
                    # snap_arglist$streams = flow_f
                }
                
                # do.call(wbt_snap_pour_points,
                #         args = snap_arglist) %>% invisible()
                
                #the site has been snapped only one way
                unique_snaps_f <- c(snap1_f)
            }
            
            #good for experimenting with snap specs:
            # delineate_watershed_test2(scratch_dir, point_f, flow_f,
            #                           d8_f, 'standard', 1000)
            
            #delineate each unique location
            for(i in 1:length(unique_snaps_f)){
                
                rgx <- str_match(unique_snaps_f[i],
                                 '.*?_(standard|jenson)_dist([0-9]+)\\.shp$')
                snap_method_ <- rgx[, 2]
                snap_dist_ <- rgx[, 3]
                
                wb_f <- glue('{path}/wb{n}_buffer{b}_{typ}_dist{dst}.tif',
                             path = scratch_dir,
                             n = i,
                             b = buffer_radius,
                             typ = snap_method_,
                             dst = snap_dist_)
                
                whitebox::wbt_watershed(d8_pntr = d8_f,
                                        pour_pts = unique_snaps_f[i],
                                        output = wb_f) %>% invisible()
                
                wb <- terra::rast(wb_f)
                
                #check how many wb cells coincide with the edge of the DEM.
                #If > 0.1% or > 5, broader DEM needed
                smry <- raster_intersection_summary(wb = wb,
                                                    dem = dem)
                
                if(verbose){
                    print(glue('site buffer radius: {br}; pour point snap: {sn}/{tot}; ',
                               'n intersecting border cells: {ni}; pct intersect: {pct}',
                               br = buffer_radius,
                               sn = i,
                               tot = length(unique_snaps_f),
                               ni = round(smry$n_intersections, 2),
                               pct = round(smry$pct_wb_cells_intersect, 2)))
                }
                
                if(smry$pct_wb_cells_intersect > 0.1 || smry$n_intersections > 5){
                    
                    buffer_radius_new <- buffer_radius * 10
                    dem_coverage_insufficient <- TRUE
                    print(glue('Hit DEM edge. Incrementing buffer.'))
                    break
                    
                } else {
                    
                    dem_coverage_insufficient <- FALSE
                    buffer_radius_new <- buffer_radius
                    
                    #write and record temp files for visual inspection
                    wb_sf <- wb %>%
                        terra::as.polygons() %>%
                        sf::st_as_sf() %>%
                        sf::st_buffer(dist = 0.1) %>%
                        sf::st_union() %>%
                        sf::st_as_sf() %>%
                        fill_sf_holes() %>%
                        sf::st_transform(4326)
                    
                    ws_area_ha <- as.numeric(sf::st_area(wb_sf)) / 10000
                    
                    wb_sf <- wb_sf %>%
                        mutate(site_code = !!site_code) %>%
                        mutate(area = !!ws_area_ha)
                    
                    if(is.null(flat_increment)){
                        flt_incrmt <- 'null'
                    } else {
                        flt_incrmt <- as.character(flat_increment)
                    }
                    
                    wb_sf_f <- glue('{path}/wb{n}_BUF{b}{typ}DIST{dst}RES{res}',
                                    'INC{inc}BREACH{brc}BURN{brn}.shp',
                                    path = inspection_dir,
                                    n = i,
                                    b = sprintf('%d', buffer_radius),
                                    typ = snap_method_,
                                    dst = snap_dist_,
                                    res = dem_resolution,
                                    inc = flt_incrmt,
                                    brc = breach_method,
                                    brn = as.character(burn_streams))
                    
                    sw(sf::st_write(obj = wb_sf,
                                    dsn = wb_sf_f,
                                    delete_dsn = TRUE,
                                    quiet = TRUE))
                }
            }
            
            buffer_radius <- buffer_radius_new
        } #end while loop
        
        if(verbose){
            message(glue('Candidate delineations are in: ', inspection_dir))
        }
        
        delin_out <- list(inspection_dir = inspection_dir,
                          buffer_radius = buffer_radius)
        
        return(delin_out)
    }
    
    if(verbose){
        message('Beginning watershed delineation')
    }
    
    tmp <- tempdir()
    
    all_specs_provided <- ! is.null(spec_buffer_radius_m) &&
        ! is.null(spec_snap_distance_m) &&
        ! is.null(spec_snap_method) &&
        ! is.null(spec_dem_resolution) &&
        ! is.null(spec_breach_method) &&
        ! is.null(spec_burn_streams)
    
    selection <- sw(delineate_watershed_apriori_recurse(
        lat = lat,
        long = long,
        crs = crs,
        site_code = write_name,
        buffer_radius = spec_buffer_radius_m,
        snap_dist = spec_snap_distance_m,
        snap_method = spec_snap_method,
        dem_resolution = spec_dem_resolution,
        flat_increment = spec_flat_increment,
        breach_method = spec_breach_method,
        burn_streams = spec_burn_streams,
        confirm = ! (all_specs_provided && ! confirm),
        scratch_dir = tmp,
        write_dir = write_dir,
        # dev_machine_status = 'n00b',
        verbose = verbose))
    
    #calculate watershed area in hectares
    wb <- sf::st_read(glue('{d}/{s}.shp',
                           d = write_dir,
                           s = write_name),
                      quiet = TRUE)
    
    ws_area_ha <- as.numeric(sf::st_area(wb)) / 10000
    
    #return the specifications of the correctly delineated watershed, and some
    #   other goodies
    rgx <- str_match(selection,
                     paste0('^wb[0-9]+_BUF([0-9]+)(standard|jenson)',
                            'DIST([0-9]+)RES([0-9]+)INC([0-1\\.null]+)',
                            'BREACH(basic|lc)BURN(TRUE|FALSE)\\.shp$'))
    
    deets <- list(name = write_name,
                  watershed_area_ha = ws_area_ha,
                  buffer_radius_m = as.numeric(rgx[, 2]),
                  snap_distance_m = as.numeric(rgx[, 4]),
                  snap_method = rgx[, 3],
                  dem_resolution = as.numeric(rgx[, 5]),
                  flat_increment = rgx[, 6],
                  breach_method = rgx[, 7],
                  burn_streams = as.logical(rgx[, 8]))
    
    return(deets)
}

ms_delineate_watershed(
