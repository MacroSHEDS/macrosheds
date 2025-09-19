#' Read MacroSheds spatial data products from disk into R.
#'
#' Load a macrosheds spatial product (ws_boundary, precip_gauge_locations,
#' or stream_gauge_locations) from a downloaded MacroSheds dataset. See
#' [ms_load_product()] for core time-series data and watershed attributes.
#'
#' @author Spencer Rhea
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, established with [ms_download_core_data()].
#' @param spatial_product character. One of "stream_gauge_locations", "precip_gauge_locations",
#'    or "ws_boundary" (watershed boundary) Files associated with the requested
#'    product will be combined across specified networks/domains.
#' @param version numeric or "latest". The MacroSheds dataset version from which to load data.
#' @param networks character vector. MacroSheds networks to load; optional. Omit networks, domains, and site_codes to load all. For a catalog of
#'    networks, use [ms_load_sites()], visit [macrosheds.org](https://macrosheds.org), or see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262)
#' @param domains character vector. MacroSheds domains to load; optional. Omit networks, domains, and site_codes to load all. For a catalog of
#'    domains, use [ms_load_sites()], visit [macrosheds.org](https://macrosheds.org), or see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262)
#' @param site_codes character vector. MacroSheds networks to load; optional. Omit networks, domains, and site_codes to load all. For a catalog of
#'    sites, use [ms_load_sites()], visit [macrosheds.org](https://macrosheds.org), or see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262)
#' @param warn logical. If TRUE, you'll get notifications about more recent versions of the MacroSheds dataset, if available.
#' @return returns an \code{sf} object containing all data belonging to the
#'    selected spatial product in the \code{macrosheds_root} directory.
#' @export
#' @seealso [ms_download_core_data()], [ms_load_product()], [ms_load_variables()], [ms_load_sites()]
#' @examples
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = c('niwot', 'hjandrews', 'hbef'))
#' macrosheds_data <- load_spatial_product(macrosheds_root = ms_root,
#'                                         spatial_product = 'ws_boundary',
#'                                         domains = 'hbef')

ms_load_spatial_product <- function(macrosheds_root,
                                    spatial_product,
                                    version = 'latest',
                                    networks,
                                    domains,
                                    site_codes,
                                    warn = TRUE){

    library("dplyr", quietly = TRUE)
    requireNamespace('macrosheds', quietly = TRUE)

    # Checks
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied')
    } else {
        macrosheds_root <- sw(normalizePath(macrosheds_root))
    }
    if(missing(spatial_product) || ! any(spatial_product %in% c('ws_boundary', 'precip_gauge_locations', 'stream_gauge_locations'))) {
        stop('Available `spatial_product`s are: "ws_boundary", "precip_gauge_locations", or "stream_gauge_locations".')
    }

    if(length(spatial_product) > 1){
        stop('only one type of `spatial_product` can be loaded at a time.')
    }

    #handle derelict files from pre-v2
    root_files <- list.files(macrosheds_root,
                             full.names = TRUE)
    root_files <- grep('/v[0-9]+$', root_files, value = TRUE, invert = TRUE)

    if(length(root_files)){
        v1path <- file.path(macrosheds_root, 'v1')
        rsp <- get_response_1char(paste0('Since v2, MacroSheds data files are stored by version. Is it okay to move all contents of ',
                                         macrosheds_root, ' into ', v1path, '? (y/n) >'),
                                  possible_chars = c('y', 'n'))
        if(rsp == 'n'){
            cat('Please set macrosheds_root to a location where only MacroSheds data files will be stored.\n')
            return(invisible())
        }

        dir.create(v1path, showWarnings = FALSE)
        file.rename(root_files, file.path(v1path, basename(root_files)))
    }

    root_vsn <- validate_version(macrosheds_root = macrosheds_root,
                                 version = version,
                                 warn = warn)

    prodpaths <- list.files(root_vsn,
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = '*.shp')

    if(length(prodpaths) == 0){
        stop('No spatial products in ', root_vsn, '. Verify macrosheds_root location or see ms_download_core_data().')
    }

    prodpaths <- grep(spatial_product, prodpaths, value = TRUE)

    # List network files
    if(! missing(networks)){
        netdom <- macrosheds::ms_site_data %>%
            dplyr::select(network, domain) %>%
            distinct(domain, .keep_all = TRUE)

        network_domains <- netdom %>%
            filter(network %in% !!networks) %>%
            pull(domain)

        prodpaths_net <- grep(paste(network_domains, collapse = '|'), prodpaths, value = TRUE)
    } else{
        prodpaths_net <- NULL
    }

    # List domain files
    if(!missing(domains)){
        prodpaths_dom <- grep(paste(domains, collapse = '|'), prodpaths, value = TRUE)
    } else{
        prodpaths_dom <- NULL
    }

    # List site files
    if(!missing(site_codes)){
        sites <- stringr::str_match(string = prodpaths,
                           pattern = '([^/]+)(?=\\.shp$)')[,1]
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
                         z <- sf::st_read(x,
                                          stringsAsFactors = FALSE,
                                          quiet = TRUE) %>%
                             slice_tail()
                         sw(sf::st_set_crs(z, 4326))
                     })

    combined <- suppressMessages(Reduce(bind_rows, shapes))

    return(combined)
}
