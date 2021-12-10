#' Load in macrosheds spatial data products
#'
#' Load an entire macrosheds spatial product (ws_boundary, precip_gauge_locations, 
#' or stream_gauge_locations) from a downloaded macrosheds dataset see 
#' \code{download_ms_code_data()} for data download
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, e.g. '~/stuff/macrosheds_dataset_v0.3'
#' @param spatial_product character. read and combine all files associated with 
#'    this spatial prodname across all networks and domains. Available spatial_product 
#'    are: ws_boundary, precip_gauge_locations, or stream_gauge_locations
#' @param domains character vector. Domains to load, optional
#' @param site_codes character vector. site_codes to load, optional
#' @return returns a \code{sf} object containing all the all data belonging to the 
#'    selected spatial product in the \code{macrosheds_root} directory
#' @export
#' @examples
#' macrosheds_data <- load_spatial_products(macrosheds_root = 'data/macrosheds_v1', 
#'                                          spatial_product = 'ws_boundary'
#'                                          domains = 'hbef')

load_spatial_products <- function(macrosheds_root,
                                    spatial_product,
                                    domains,
                                    site_codes){
    
    # Checks 
    if(missing(macrosheds_root)) {
        stop(print('macrosheds_root must be supplied'))
    }
    if(missing(spatial_product) || ! spatial_product %in% c('ws_boundary', 'precip_gauge_locations', 'stream_gauge_locations')) {
        stop(print('a spatial_product of ws_boundary, precip_gauge_locations, or stream_gauge_locations must be supplied'))
    }
    
    prodpaths <- list.files(macrosheds_root,
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = '*.shp')
    
    if(length(prodpaths) == 0){
        stop(print('No spatial products in macrosheds_root, check macrosheds_root is correct or files exist'))
    }
    
    prodpaths <- grep(spatial_product, prodpaths, value = TRUE)
    
    if(!missing(domains)){
        prodpaths_dom <- grep(paste(domains, collapse = '|'), prodpaths, value = TRUE)
    } else{
        prodpaths_dom <- NULL
    }
    
    if(!missing(site_codes)){
        sites <- str_match(string = prodpaths,
                           pattern = '([^/]+)(?=\\.shp$)')[,1]
        prodpaths_sites <- prodpaths[grep(paste0(site_codes, collapse = '|'), sites)]
    } else{
        prodpaths_sites <- NULL
    }
    
    if(!missing(domains) || !missing(site_codes)){
        prodpaths <- c(prodpaths_sites, prodpaths_dom)
        prodpaths <- unique(prodpaths)
    } 
    
    
    shapes <- lapply(prodpaths,
                     function(x){
                         sf::st_read(x,
                                     stringsAsFactors = FALSE,
                                     quiet = TRUE) %>%
                             slice_tail()
                     })
    
    # wb <- sw(Reduce(sf::st_union, wbs)) %>%
    combined <- suppressMessages(Reduce(bind_rows, shapes))
    # sf::st_transform(projstring)
    
    return(combined)
}

