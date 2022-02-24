#' Identify USGS gauges near a point
#'
#' Search for USGS gauges within a radius of a location specified by latitude and longitude
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param sites Either a \code{sf} object of sites of interest or a \code{data.frame}
#'    containing latitude and longitude information.
#' @param lat Character. If sites is not a \code{sf} object, the name of the column
#'    containing the latitude locations in WGS84. Default NULL.
#' @param long Character. If sites is not a \code{sf} object, the name of the column
#'    containing the longitude locations in WGS84. Default NULL.
#' @param radius Numeric. The radius in meters that a USGS gauge will be search
#'    for from a point in the sites file. Default 500.
#' @return returns a \code{data.frame} with a the column usgs_site.
#' @details The function will search for a USGS gauge within the radius supplied 
#'    for every site in the sites file. The dataRetrieval package is used to locate USGS gauges.
#'    If no USGS gauge is found, then the usgs_site column will be NA for that site. 
#' @export
#' @examples
#' # With lat long 
#' sites <- tibble(site_code = c('MA_AE03', 'MI_KR01'),
#'                 Latitude = c(42.04856, 42.28575),
#'                 Longitude = c(-72.45403, -85.51467))
#' ms_identify_usgs_gauges(sites = sites, lat = 'Latitude', long = 'Longitude')
#' # With sf
#' site_sf <- sites %>%
#'     sf::st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#'     sf::st_buffer(radius)


ms_identify_usgs_gauges <- function(sites, lat = NULL, long = NULL, radius = 500) {
    
    if((is.null(lat) && !is.null(long)) || (!is.null(lat) && is.null(long))) {
        stop('Both lat and long must be provided or a sf objust must be the input to site')
    }
    if(radius >= 100000) {
        stop('radius must be less than 100000 m')
    }
    
    if(is.null(lat) && is.null(long)){
        use_sf <- TRUE
        sites <- sites %>%
            sf::st_transform(., crs = 4326)
    } else{
        use_sf = FALSE
    }
    
    
    for(i in 1:nrow(sites)){
        if(use_sf){
            point <- sites[i,] %>%
                sf::st_buffer(radius)
        } else{
            latitude <- as.numeric(sites[i,lat])
            longitude <- as.numeric(sites[i,long])
            point <- tibble(lat = latitude,
                            long = longitude) %>%
                sf::st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
                sf::st_buffer(radius)
        }
        
        bbox <- round(as.numeric(sf::st_bbox(point)), 4)
        bbox <- c(bbox[1], bbox[2], bbox[3], bbox[4])
        
        usgs_sites <- try(suppressMessages(dataRetrieval::whatNWISsites(bBox = bbox,
                                                       parameterCd=c("00010","00060"),
                                                       hasDataTypeCd="dv")),
                          silent = TRUE)
        if(is.null(usgs_sites) || inherits(usgs_sites, 'try-error')) {
            sw(sites$usgs_site[i] <- NA)
        } else{
            sw(sites$usgs_site[i] <- usgs_sites$site_no)
        }
    }
    
    return(sites)
}

