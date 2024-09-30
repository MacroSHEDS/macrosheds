#' @noRd
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

    if(! file.exists(to_dir)){
        tryCatch({
            dir.create(to_dir, recursive = TRUE)
        }, warning = function(w) stop(w))
    }

    for(i in 1:length(shp_files)){

        shapefile_base <- strsplit(shp_files[i], '\\.shp')[[1]]

        files_to_move <- list.files(path = from_dir,
                                    pattern = shapefile_base)

        extensions <- stringr::str_match(files_to_move,
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
                                                    to = glue::glue('{td}/{n}{ex}',
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
                                                  to = glue::glue('{td}/{n}{ex}',
                                                            td = to_dir,
                                                            n = nm,
                                                            ex = ext),
                                                  overwrite = TRUE),
                   x = files_to_move,
                   nm = new_name_base,
                   ext = extensions)

            lapply(file.path(from_dir, files_to_move),
                   unlink)
        })
    }
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
                                                verbose = FALSE,
                                                responses_from_file){

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
        verbose = verbose)

    inspection_dir <- delin_out$inspection_dir

    files_to_inspect <- list.files(path = inspection_dir,
                                   pattern = '.shp')

    if(! confirm){

        message(glue::glue('Delineation successful and confirm == FALSE. Writing shapefile to ',
                           write_dir))

        move_shapefiles(shp_files = files_to_inspect[1],
                        from_dir = inspection_dir,
                        to_dir = write_dir,
                        new_name_vec = site_code)

        return(files_to_inspect[1])
    }

    # temp_point <- glue::glue(scratch_dir, '/', 'POINT')

    temp_point <- tibble(longitude = long, latitude = lat) %>%
        sf::st_as_sf(coords = c('longitude', 'latitude'),
                     crs = crs)
        # sf::st_write(dsn = temp_point,
        #              driver = 'ESRI Shapefile',
        #              delete_dsn = TRUE,
        #              quiet = TRUE)

    nshapes <- length(files_to_inspect)
    numeric_selections <- paste('Accept delineation', 1:nshapes)

    wb_selections <- paste(paste0('[',
                                  c(1:nshapes, 'M', 'D', 'S', 'B', 'U', 'R', 'I', 'a'),
                                  ']'),
                           c(numeric_selections,
                             'Select pourpoint snapping method',
                             'Set pourpoint snapping maximum distance (meters)',
                             'Burn streams into the DEM (may help delineator across road-stream intersections)',
                             'Use more aggressive breaching method (temporary default, pending whitebox bugfix)',
                             'Set buffer radius (distance from pourpoint to include in DEM download; meters)',
                             'Select DEM resolution',
                             'Set flat_increment',
                             # 'Next (skip this one for now, if you\'re delineating in a loop)',
                             'Abort delineation. Silently returns NULL. Ignores other selections.'),
                           sep = ': ',
                           collapse = '\n')

    # helper_code <- glue::glue('{id}.\nmapview::mapviewOptions(fgb = FALSE);',
    #                     'mapview::mapview(sf::st_read("{wd}/{f}")) + ',
    #                     'mapview::mapview(sf::st_read("{pf}"))',
    #                     id = 1:length(files_to_inspect),
    #                     wd = inspection_dir,
    #                     f = files_to_inspect,
    #                     pf = temp_point) %>%
    #     paste(collapse = '\n\n')

    msg <- glue::glue('\n\nVisually inspect the watershed boundary candidate(s).\n',
                # 'by pasting the mapview lines below into a separate instance of R.\n\n{hc}\n\n',
                'Enter the number corresponding to the ',
                'one that looks best, or select one or more tuning ',
                'options (e.g. MDSBUR).\n\nChoices:\n{sel}\n\nEnter choice(s) here > ',
                # hc = helper_code,
                sel = wb_selections)

    # mapview::mapviewOptions(fgb = FALSE);',
    for(i in seq_along(files_to_inspect)){

        mpv <- mapview::mapview(sf::st_read(file.path(inspection_dir,
                                                      files_to_inspect[i]),
                                            quiet = TRUE),
                                layer.name = paste('Candidate watershed', i)) +
            mapview::mapview(temp_point,
                             legend = FALSE,
                             layer.name = 'Input lat long') +
            mapview::mapview(sf::st_read(delin_out$unique_snaps_f[i]),
                             legend = FALSE,
                             layer.name = 'Snapped pour point')

        print(mpv)
        if(i != length(files_to_inspect)){
            get_response_enter(paste('\nPress [enter/return] to see candidate',
                                     i + 1),
                               response_from_file = responses_from_file)
        }
    }

    resp <- get_response_mchar(
        msg = msg,
        possible_resps = paste(c(1:nshapes, 'M', 'D', 'S', 'B', 'U', 'R', 'I', 'n', 'a'),
                               collapse = ''),
        allow_alphanumeric_response = FALSE,
        response_from_file = responses_from_file)

    if('a' %in% resp){

        message(glue::glue('Aborted delineation.'))

        abortdelin <- 1
        class(abortdelin) <- 'abort_delin'
        return(abortdelin)
    }

    if('M' %in% resp){
        snap_method <- get_response_1char(
            msg = paste0('Standard snapping moves the pourpoint to the cell with ',
                         'the highest flow accumulation within snap_dist. Jenson ',
                         'method tries to snap to the nearest cell identified as part of a flowline.\n\n',
                         '1. Jenson\n2. Standard\n\n',
                         'Enter choice here > '),
            possible_chars = paste(1:2),
            response_from_file = responses_from_file)
        snap_method <- ifelse(snap_method == '1', 'jenson', 'standard')
    }

    if('D' %in% resp){
        snap_dist <- get_response_int(
            msg = paste0('Enter a snap distance between 0 and 200 (meters) > '),
            min_val = 0,
            max_val = 200,
            response_from_file = responses_from_file)
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
            max_val = 100000,
            response_from_file = responses_from_file)
    }

    if('R' %in% resp){
        dem_resolution <- get_response_mchar(
            msg = paste0('Choose DEM resolution between 1 (low) and 14 (high)',
                         ' to pass to elevatr::get_elev_raster. For tiny ',
                         'watersheds, use 12-13. For big ones, use 8-9.\n\n',
                         'Enter choice here > '),
            possible_resps = paste(1:14),
            response_from_file = responses_from_file)
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
            msg = glue::glue('Pick the size of the elevation increment to pass to ',
                       bm, '.\n\n', new_options, '\n\nEnter choice here > '),
            possible_chars = c('S', 'M', 'L'),
            response_from_file = responses_from_file)

        flat_increment <- switch(resp2,
                                 S = 0.001,
                                 M = 0.01,
                                 L = 0.1)
    }

    if(! any(grepl('[0-9]', resp))){

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
            verbose = verbose,
            responses_from_file = responses_from_file)

        return(selection)
    }

    selection <- files_to_inspect[as.numeric(resp)]

    move_shapefiles(shp_files = selection,
                    from_dir = inspection_dir,
                    to_dir = write_dir,
                    new_name_vec = site_code)

    message(glue::glue('Selection {s}:\n\t{sel}\nwas written to:\n\t{sdr}/\nas {nm}',
                 '.shp, {nm}.shx, {nm}.dbf, and {nm}.prj',
                 s = resp,
                 sel = selection,
                 sdr = write_dir,
                 nm = site_code))

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
    #verbose: logical. determines the amount of informative messaging during run

    #returns the location of candidate watershed boundary files

    # tmp <- tempdir()
    # tmp <- stringr::str_replace_all(tmp, '\\\\', '/')

    if(! is.null(dem_resolution) && ! is.numeric(dem_resolution)){
        stop('dem_resolution must be an integer from 1 to 14, or NULL.')
    }
    if(! is.null(flat_increment) && ! is.numeric(flat_increment)){
        stop('flat_increment must be numeric or NULL. Recommended values are 0.1, 0.01, or 0.001.')
    }
    if(! is.null(snap_dist) && ! is.numeric(snap_dist)){
        stop('snap_dist must be numeric (meters) or NULL')
    }
    if(! is.null(buffer_radius) && ! is.numeric(buffer_radius)){
        stop('buffer_radius must be numeric (meters) or NULL')
    }
    if(! is.null(snap_method) && ! snap_method %in% c('jenson', 'standard')){
        stop('snap_dist must be "jenson", "standard", or NULL')
    }
    if(! breach_method %in% c('lc', 'basic')) stop('breach_method must be "basic" or "lc"')
    if(! is.logical(burn_streams)) stop('burn_streams must be TRUE or FALSE')

    inspection_dir <- file.path(scratch_dir, 'INSPECT_THESE')
    point_dir <- file.path(scratch_dir, 'POINT')
    dem_f <- file.path(scratch_dir, 'dem.tif')
    point_f <- file.path(scratch_dir, 'point.shp')
    streams_f <- file.path(scratch_dir, 'streams.shp')
    roads_f <- file.path(scratch_dir, 'roads.shp')
    d8_f <- file.path(scratch_dir, 'd8_pntr.tif')
    flow_f <- file.path(scratch_dir, 'flow.tif')

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
            dem_resolution <- 9
        }

        if(verbose){

            if(is.null(flat_increment)){
                fi <- 'auto'
            } else {
                fi <- as.character(flat_increment)
            }

            print(glue::glue('Delineation specs for this attempt:\n',
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

        site_buf <- sf::st_buffer(x = sf::st_transform(site, get_utm_crs(site)),
                                  dist = buffer_radius) %>%
            sf::st_transform(sf::st_crs(site))

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

            snap1_f <- glue::glue(scratch_dir, '/snap1_jenson_dist150.shp')
            whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                  streams = flow_f,
                                                  output = snap1_f,
                                                  snap_dist = 150) %>% invisible()
            snap2_f <- glue::glue(scratch_dir, '/snap2_standard_dist50.shp')
            whitebox::wbt_snap_pour_points(pour_pts = point_f,
                                           flow_accum = flow_f,
                                           output = snap2_f,
                                           snap_dist = 50) %>% invisible()
            snap3_f <- glue::glue(scratch_dir, '/snap3_standard_dist150.shp')
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

            snap1_f <- glue::glue('{scrd}/snap1_{smet}_dist150.shp',
                            scrd = scratch_dir,
                            smet = snap_method)

            snap2_f <- glue::glue('{scrd}/snap2_{smet}_dist50.shp',
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

            snap1_f <- glue::glue('{scrd}/snap1_jenson_dist{sdst}.shp',
                            scrd = scratch_dir,
                            sdst = snap_dist)

            whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                                  streams = flow_f,
                                                  output = snap1_f,
                                                  snap_dist = snap_dist) %>% invisible()

            snap2_f <- glue::glue('{scrd}/snap2_standard_dist{sdst}.shp',
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

            snap1_f <- glue::glue('{scrd}/snap1_{smet}_dist{sdst}.shp',
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

            rgx <- stringr::str_match(unique_snaps_f[i],
                             '.*?_(standard|jenson)_dist([0-9]+)\\.shp$')
            snap_method_ <- rgx[, 2]
            snap_dist_ <- rgx[, 3]

            wb_f <- glue::glue('{path}/wb{n}_buffer{b}_{typ}_dist{dst}.tif',
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
                message(glue::glue('site buffer radius: {br}; pour point snap: {sn}/{tot}; ',
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
                message(glue::glue('Hit DEM edge. Incrementing buffer.'))
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

                wb_sf_f <- glue::glue('{path}/wb{n}_BUF{b}{typ}DIST{dst}RES{res}',
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
        message(glue::glue('Candidate delineations are in: ', inspection_dir))
    }

    delin_out <- list(inspection_dir = inspection_dir,
                      buffer_radius = buffer_radius,
                      unique_snaps_f = unique_snaps_f)

    return(delin_out)
}

get_utm_crs <- function(point){

    lon <- sf::st_coordinates(point)[1, 1]
    lat <- sf::st_coordinates(point)[1, 2]

    zone_number <- floor((lon + 180) / 6) + 1
    zone_hemisphere <- ifelse(lat >= 0, 'N', 'S')

    zone <- paste0(zone_number, zone_hemisphere)
    crs <- paste0("EPSG:", ifelse(grepl('N', zone), 32600, 32700) + zone_number)

    return(crs)
}
