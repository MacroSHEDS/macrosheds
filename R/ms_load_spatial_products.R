#' Load in macrosheds spatial data products
#'
#' Load a macrosheds spatial product (ws_boundary, precip_gauge_locations, 
#' or stream_gauge_locations) from a downloaded macrosheds dataset see 
#' \code{download_ms_code_data()} for data download.
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, e.g. '~/stuff/macrosheds_dataset_v0.3'.
#' @param spatial_product character. read and combine files associated with 
#'    this spatial prodname across selected networks and domains. Available spatial_product 
#'    are: ws_boundary, precip_gauge_locations, or stream_gauge_locations.
#' @param networks character vector. networks to load, optional.
#' @param domains character vector. Domains to load, optional.
#' @param site_codes character vector. site_codes to load, optional.
#' @return returns a \code{sf} object containing all the all data belonging to the 
#'    selected spatial product in the \code{macrosheds_root} directory.
#' @export
#' @examples
#' macrosheds_data <- load_spatial_products(macrosheds_root = 'data/macrosheds_v1', 
#'                                          spatial_product = 'ws_boundary'
#'                                          domains = 'hbef')

ms_load_spatial_products <- function(macrosheds_root,
                                     spatial_product,
                                     networks,
                                     domains,
                                     site_codes){
    
    # Checks 
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied')
    }
    if(missing(spatial_product) || ! spatial_product %in% c('ws_boundary', 'precip_gauge_locations', 'stream_gauge_locations')) {
        stop('a spatial_product of ws_boundary, precip_gauge_locations, or stream_gauge_locations must be supplied')
    }
    
    if(length(spatial_product) > 1){
        stop('only one spatial product can be loaded at a time')
    }
    
    prodpaths <- list.files(macrosheds_root,
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = '*.shp')
    
    if(length(prodpaths) == 0){
        stop('No spatial products in macrosheds_root, check macrosheds_root is correct or files exist')
    }
    
    prodpaths <- grep(spatial_product, prodpaths, value = TRUE)
    
    # List network files  
    if(!missing(networks)){
        netdom <- ms_download_site_data() %>%
            select(network, domain) %>%
            distinct(domain, .keep_all = TRUE)
        
        network_domains <- netdom %>%
            filter(network %in% !!networks) %>%
            pull(domain)
        
        # prodpaths_net <- grep(paste0(paste0(macrosheds_root, '/', network_domains, '/', spatial_product), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        
        prodpaths_net <- grep(paste(network_domains, collapse = '|'), prodpaths, value = TRUE)
    } else{
        prodpaths_net <- NULL
    }
    
    # List domain files 
    if(!missing(domains)){
        # prodpaths_dom <- grep(paste0(paste0(macrosheds_root, '/', domains, '/', spatial_product), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        prodpaths_dom <- grep(paste(domains, collapse = '|'), prodpaths, value = TRUE)
    } else{
        prodpaths_dom <- NULL
    }
    
    # List site files
    if(!missing(site_codes)){
        sites <- str_match(string = prodpaths,
                           pattern = '([^/]+)(?=\\.shp$)')[,1]
        # prodpaths_sites <- prodpaths[grep(paste0(site_codes, collapse = '|'), sites)]
        prodpaths_sites <- prodpaths[sites %in% site_codes]
    } else{
        prodpaths_sites <- NULL
    }
    
    if(!missing(domains) || !missing(site_codes) || !missing(networks)){
        prodpaths <- c(prodpaths_sites, prodpaths_dom, prodpaths_net)
        prodpaths <- unique(prodpaths)
    } 
    
    
    shapes <- lapply(prodpaths,
                     function(x){
                         sf::st_read(x,
                                     stringsAsFactors = FALSE,
                                     quiet = TRUE) %>%
                             slice_tail()
                     })
    
    combined <- suppressMessages(Reduce(bind_rows, shapes))
    
    return(combined)
}

