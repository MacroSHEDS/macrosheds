#' Delineate a watershed from a stream location.
#' 
#' From any geographic location, this function (iteratively) attempts to delineate a watershed,
#' i.e. the land area that contributes overland flow to that location. You will need to install whitebox
#' binaries via \code{whitebox::install_whitebox()} before it will work. Delineation
#' can be guided by user knowledge of local geography, or it can be fully agnostic.
#' Any specifications not explicitly passed are given reasonable defaults. Candidate
#' delineations are made available for viewing and the user is asked to pick one,
#' or try again by interactively providing different specifications. This function
#' is intended to be used interactively in Rstudio. If you'd like to use it in some
#' other fashion, let us know. See details.
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
#' @param spec_buffer_radius_m: optional integer. the radius, in meters, of the buffer circle around the site location.
#' A DEM will be acquired that covers at least the full area of the buffer, so
#' basically a square of DEM in which that circle is inscribed. Must be between 1000
#' and 100,000 meters, though your watershed may be larger (in which case the buffer will be
#' incremented automatically. This is set initially to 1000 meters if NULL.
#' @param spec_snap_distance_m: optional integer \[0,200\]. the radius, in meters, of a circle around the recorded site location.
#' A pour point will be identified within this circle. method depends on \code{snap_method}.
#' Several reasonable values chosen if NULL.
#' @param spec_snap_method: optional character. either "standard", which snaps the site location
#' to the cell within snap_dist that has the highest flow value, or
#' "jenson", which snaps to the nearest cell identified as part of a flow path.
#' Because we use a flow accumulation raster in place of a streams raster, this
#' isn't all that different from "standard," but it can be a helpful way to
#' "jiggle" the snapping algorithm if it's struggling to find your stream.
#' Both methods will be attempted if NULL. Note that neither snap method will work as
#' desired if your pour point is in an estuary (delineation will proceed into the ocean).
#' @param spec_dem_resolution: optional integer \[1,14\]. the granularity of the DEM that is used for
#' delineation. this argument is passed directly to the z parameter of
#' [elevatr::get_elev_raster()]. 1 is low resolution; 14 is high. 8-9 is generally good for
#' large watersheds and 12-13 for small ones. If NULL, initial
#' dem_resolution will be set to 9.
#' @param spec_flat_increment: optional float. Passed to
#' [whitebox::wbt_breach_depressions_least_cost()]
#' or [whitebox::wbt_breach_depressions()], depending on the value
#' of \code{breach_method} (see next). Think of this as an arbitrary tuning parameter,
#' to be adjusted only if your watershed won't delineate and you've tried everything else.
#' @param spec_breach_method: optional string. Either 'basic', which invokes [whitebox::wbt_breach_depressions()],
#' or 'lc', which invokes [whitebox::wbt_breach_depressions_least_cost()]. NOTE: at present,
#' \code{wbt_breach_depressions} is used regardless of the argument to this parameter, as the least cost
#' algorithm in \code{whitebox} is non-deterministic. 
#' @param spec_burn_streams: logical. if TRUE, both [whitebox::wbt_burn_streams_at_roads()]
#' and [whitebox::wbt_fill_burn()] are called on the DEM, using road and stream
#' layers from \code{OpenStreetMap}. This allows delineation to proceed through
#' bridges, dams, and other would-be obstacles.
#' @param verbose logical. Determines the amount of informative messaging during run.
#' @param confirm logical. Ignored unless all delineation parameters
#' (the ones that start with "spec_") are supplied.
#' If TRUE, you will be asked to visually confirm the delineation before it is
#' written to \code{write_dir}.
#' @param responses_from_file character. Path to a file containing responses to
#' \code{ms_delineate_watershed} internal prompts--one per line. The file must end with
#' a newline, and it will be emptied, line-by-line from the top, during operation.
#' This is used for testing and is unlikely to be useful to end-users, but who knows?
#' @return 
#' Writes a shapefile to \code{write_dir}. Also returns a list containing the following components:
#' + watershed_area_ha: the area of the delineated watershed in hectares
#' + specs: a list of specifications of the successful delineation
#'   + buffer_radius_m: the width (meters) around the site location that was used when
#'        requesting a DEM (digital elevation model).
#'   + snap_distance_m: the search radius (meters) around the supplied lat/long that was used
#'        to snap a pour point.
#'   + snap_method: either "standard", which snaps the pour point to the cell
#'        within snap_distance_m with highest flow accumulation, or "jenson",
#'        which snaps to the nearest flow line.
#'   + dem_resolution: passed to [elevatr::get_elev_raster()] (z parameter).
#'   + flat_increment: see [whitebox::wbt_breach_depressions()] or
#'        [whitebox::wbt_breach_depressions_least_cost()].
#'   + breach_method: Either 'basic', indicating that
#'        [whitebox::wbt_breach_depressions()] was used to condition the DEM, or 'lc', indicating
#'        that [whitebox::wbt_breach_depressions_least_cost()] was used (which less readily
#'        alters the DEM).
#'   + burn_streams: TRUE or FALSE, indicating whether
#'        [whitebox::wbt_burn_streams_at_roads()] and [whitebox::wbt_fill_burn()] were
#'        used to condition the DEM.
#' @details
#' Output files are unprojected (WGS 84), though processing is done
#' on projected data. A projection is chosen automatically by [macrosheds::choose_projection()],
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
#' @seealso [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @examples
#' #here's the agnostic approach, if you're delineating this site for the first time.
#' out <- ms_delineate_watershed(
#'     lat = 44.21013,
#'     long = -122.2571,
#'     crs = 4326,
#'     write_dir = '/your/path',
#'     write_name = 'example_site'
#' )
#' 
#' #or maybe you know the watershed is on the order of 10,000 km^2 and that.
#' #there are large highway bridges over some reaches.
#' out <- ms_delineate_watershed(
#'     lat = 44.21013,
#'     long = -122.2571,
#'     crs = 4326,
#'     write_dir = '/your/path',
#'     write_name = 'example_site',
#'     spec_buffer_radius_m = 10000,
#'     spec_burn_streams = TRUE,
#')
#'
#' #or maybe you've delineated this site before and you know which specs were successful.
#' out <- ms_delineate_watershed(
#'     lat = 44.21013,
#'     long = -122.2571,
#'     crs = 4326,
#'     write_dir = '/your/path',
#'     write_name = 'example_site',
#'     spec_buffer_radius_m = 1000,
#'     spec_snap_distance_m = 150,
#'     spec_snap_method = 'standard',
#'     spec_dem_resolution = 10,
#'     spec_flat_increment = 0.01,
#'     spec_breach_method = 'basic',
#'     spec_burn_streams = FALSE,
#'     verbose = FALSE,
#'     confirm = FALSE
#')
#' @export

ms_delineate_watershed <- function(lat,
                                   long,
                                   crs = 4326,
                                   write_dir,
                                   write_name,
                                   spec_buffer_radius_m = NULL,
                                   spec_snap_distance_m = NULL,
                                   spec_snap_method = NULL,
                                   spec_dem_resolution = NULL,
                                   spec_flat_increment = NULL,
                                   spec_breach_method = 'basic',
                                   spec_burn_streams = FALSE,
                                   verbose = TRUE,
                                   confirm = TRUE,
                                   responses_from_file = NULL){

    library("dplyr", quietly = TRUE)

    # check for install of "suggested" package necessary for this function
    if(!require('terra')) {
      stop('the package "terra" is required to use this function. run install.packages("terra") and try again')
    }
    if(!require('mapview')) {
      stop('the package "mapview" is required to use this function. run install.packages("mapview") and try again')
    }
    if(!require('elevatr')) {
      stop('the package "elevatr" is required to use this function. run install.packages("elevatr") and try again')
    }
    if(!require('whitebox')) {
      stop('the package "whitebox" is required to use this function. run install.packages("whitebox")',
           ' and then install additional necessary binaries using whitebox::install_whitebox(), and then try again')
    } else {
      warning('for complete functionality make sure you have run whitebox::install_whitebox() in addition to',
              'normal package install')
    }
    if(!require('raster')) {
      stop('the package "raster" is required to use this function. run install.packages("raster") and try again')
    }

    if(missing(write_dir) || is.null(write_dir)){
        stop('write_dir must be provided')
    }
    if(missing(write_name) || is.null(write_name)){
        stop('write_name must be provided')
    }
    if(missing(crs) || is.null(crs)){
        stop('crs must be provided')
    }
    
    if(spec_breach_method == 'lc'){
        spec_breach_method <- 'basic'
        message('spec_breach_method = "lc" is currently unavailable. Setting spec_breach_method = "basic"')
    }

    requireNamespace('macrosheds', quietly = TRUE)
    
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
    
    if(! confirm && ! all_specs_provided){
        message('confirm is FALSE but required specs parameters not provided. Setting to TRUE. Note that flat_increment need not be provided.')
        confirm <- TRUE
    }
    
    selection <- try({
        sw(delineate_watershed_apriori_recurse(
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
            confirm = confirm,
            # confirm = ! (all_specs_provided && ! confirm),
            scratch_dir = tmp,
            write_dir = write_dir,
            verbose = verbose,
            responses_from_file = responses_from_file))
    })
    
    if(inherits(selection, 'abort_delin')){
        return(invisible(NULL))
    }
    
    if(inherits(selection, 'try-error')){
        message(paste('See additional error details, but note that if you',
                      'haven\'t already run whitebox::install_whitebox(),',
                      'that\'s the solution!'))
        stop(selection)
    }
    
    #calculate watershed area in hectares
    wb <- sf::st_read(glue::glue('{d}/{s}.shp',
                           d = write_dir,
                           s = write_name),
                      quiet = TRUE)
    
    ws_area_ha <- as.numeric(sf::st_area(wb)) / 10000
    
    #return the specifications of the correctly delineated watershed, and some
    #   other goodies
    rgx <- stringr::str_match(selection,
                     paste0('^wb[0-9]+_BUF([0-9]+)(standard|jenson)',
                            'DIST([0-9]+)RES([0-9]+)INC([0-1\\.null]+)',
                            'BREACH(basic|lc)BURN(TRUE|FALSE)\\.shp$'))
    
    deets <- list(name = write_name,
                  buffer_radius_m = as.numeric(rgx[, 2]),
                  snap_distance_m = as.numeric(rgx[, 4]),
                  snap_method = rgx[, 3],
                  dem_resolution = as.numeric(rgx[, 5]),
                  flat_increment = rgx[, 6],
                  breach_method = rgx[, 7],
                  burn_streams = as.logical(rgx[, 8]))
    
    return(list(out_path = write_dir,
                filename_base = write_name,
                watershed_area_ha = ws_area_ha,
                deets = deets))
}
