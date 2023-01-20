#' Read MacroSheds spatial data products from disk into R.
#'
#' Load a macrosheds spatial product (ws_boundary, precip_gauge_locations, 
#' or stream_gauge_locations) from a downloaded MacroSheds dataset. See 
#' [ms_load_product()] for time-series data.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, established with [ms_download_core_data()].
#' @param spatial_product character. Either "ws_boundary" for watershed boundary,
#'    "stream_gauge_locations", or "precip_gauge_locations". Files associated with this
#'    product will be combined across specified networks/domains.
#' @param networks character vector. MacroSheds networks to load; optional. To see a catalog of 
#'    networks, visit macrosheds.org or see [MacroSheds documentation](https://doi.org/10.6084/m9.figshare.c.5621740).
#' @param domains character vector. MacroSheds domains to load; optional. To see a catalog of 
#'    networks, visit macrosheds.org or see [MacroSheds documentation](https://doi.org/10.6084/m9.figshare.c.5621740).
#' @param site_codes character vector. MacroSheds networks to load; optional. To see a catalog of 
#'    networks, visit macrosheds.org or see [MacroSheds documentation](https://doi.org/10.6084/m9.figshare.c.5621740).
#' @return returns an \code{sf} object containing all data belonging to the 
#'    selected spatial product in the \code{macrosheds_root} directory.
#' @export
#' @seealso [ms_download_core_data()], [ms_load_product()], [ms_load_variables()], [ms_load_sites()]
#' @examples
#' ms_root = 'data/macrosheds'
#' dir.create(ms_root, recursive = TRUE)
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = c('niwot', 'hjandrews', 'hbef'))
#' macrosheds_data <- load_spatial_product(macrosheds_root = ms_root, 
#'                                         spatial_product = 'ws_boundary'
#'                                         domains = 'hbef')

ms_load_spatial_product <- function(macrosheds_root,
                                    spatial_product,
                                    networks,
                                    domains,
                                    site_codes){
    
    # Checks 
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied')
    }
    if(missing(spatial_product) || ! any(spatial_product %in% c('ws_boundary', 'precip_gauge_locations', 'stream_gauge_locations'))) {
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
        netdom <- ms_site_data %>%
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
        sites <- stringr::str_match(string = prodpaths,
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

