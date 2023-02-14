#' Read MacroSheds data products from disk into R
#'
#' Loads and optionally filters MacroSheds time-series products (e.g stream_chemistry, discharge, etc.) from a 
#' downloaded MacroSheds dataset. For watershed boundaries and gauge locations, see [ms_load_spatial_product()].
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the MacroSheds dataset's parent
#'    directory, established with [ms_download_core_data()] and/or [ms_download_ws_attr()].
#'    If you specified different locations with each of these functions, you'll need to
#'    refer to them separately when loading time-series data vs. watershed attribute data.
#' @param prodname character. A MacroSheds product name. Files associated with this
#'    product name will be read and combined. Available prodnames are (for core time-series products):
#'    
#'     + discharge
#'     + stream_chemistry
#'     + stream_flux_inst
#'     + precipitation,
#'     + precip_chemistry
#'     + precip_flux_inst
#'     
#'    (and for watershed attribute products):
#'    
#'     + ws_attr_summaries
#'     + ws_attr_timeseries:climate
#'     + ws_attr_timeseries:hydrology
#'     + ws_attr_timeseries:landcover
#'     + ws_attr_timeseries:parentmaterial
#'     + ws_attr_timeseries:terrain
#'     + ws_attr_timeseries:vegetation
#'     + ws_attr_CAMELS_summaries
#'     + ws_attr_CAMELS_Daymet_forcings 
#' @param filter_vars character vector. for products like stream_chemistry that include
#'    multiple variables, this filters to just the ones specified (ignores
#'    variable prefixes). Ignored if requesting discharge, precipitation, or watershed attributes.
#'    To see a catalog of variables, visit macrosheds.org or see [MacroSheds documentation](EDI link pending).
#' @param networks character vector. MacroSheds networks to load; optional. To see a catalog of 
#'    networks, visit macrosheds.org or see [MacroSheds documentation](EDI link pending).
#' @param domains character vector. MacroSheds domains to load; optional. To see a catalog of
#'    domains, visit macrosheds.org or see [MacroSheds documentation](EDI link pending).
#' @param site_codes character vector. MacroSheds sites to load, optional. To see a catalog of
#'    site_codes, visit macrosheds.org or see [MacroSheds documentation](EDI link pending).
#' @param sort_result logical. Ignored if requesting watershed attributes.
#'    If TRUE, and requesting core time-series data, output will be sorted by site_code, var,
#'    datetime. this may add considerable loading time for large datasets.
#' @param warn logical. If TRUE, function will not load more than 100 MB without permission.
#' @return Returns a \code{tibble} in MacroSheds format. See [MacroSheds documentation](EDI link pending) for definitions.
#' @export
#' @seealso [ms_download_core_data()], [ms_download_ws_attr()], [ms_load_spatial_product()], [ms_load_variables()], [ms_load_sites()]
#' @examples
#' ms_root = 'data/macrosheds'
#' dir.create(ms_root, recursive = TRUE)
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = c('niwot', 'hjandrews'))
#' macrosheds_data <- ms_load_product(macrosheds_root = ms_root, 
#'                                    prodname = 'stream_chemistry', 
#'                                    filter_vars = 'NO3_N')

ms_load_product <- function(macrosheds_root, 
                            prodname,
                            filter_vars,
                            networks,
                            domains,
                            site_codes,
                            sort_result = FALSE,
                            warn = TRUE){

    library("dplyr", quietly = TRUE)

    requireNamespace('macrosheds', quietly = TRUE)
    
    # Checks 
    avail_prodnames <- c('discharge', 'stream_chemistry', 'stream_flux_inst_scaled',
                         'precipitation', 'precip_chemistry', 'precip_flux_inst_scaled',
                         'ws_attr_summaries', 'ws_attr_timeseries:climate',
                         'ws_attr_timeseries:hydrology', 'ws_attr_timeseries:landcover',
                         'ws_attr_timeseries:parentmaterial', 'ws_attr_timeseries:terrain',
                         'ws_attr_timeseries:vegetation', 'ws_attr_CAMELS_Daymet_forcings',
                         'ws_attr_CAMELS_summaries')

    if(missing(macrosheds_root)){ 
        stop('macrosheds_root must be supplied.')
    }
    if(! dir.exists(macrosheds_root)){
        stop('macrosheds_root does not exist. This should be the directory you specified when you ran ms_download_core_data or ms_download_ws_attr.')
    }
    if(missing(prodname)){
        stop('prodname must be supplied')
    }
    if(! inherits(prodname, 'character') || length(prodname) != 1){
        stop('prodname must be a character string')
    }
    if(! prodname %in% avail_prodnames){
        stop(paste0('prodname must be one of: "',
                    paste(avail_prodnames, collapse = '", "'),
                    '".'))
    }
    if(! missing(networks)){
        ntws <- unique(macrosheds::ms_site_data$network)
        if(any(! networks %in% ntws)){
            illeg <- Filter(function(x) ! x %in% ntws, networks)
            stop('illegal network names: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    if(! missing(domains)){
        dmns <- unique(macrosheds::ms_site_data$domain)
        if(any(! domains %in% dmns)){
            illeg <- Filter(function(x) ! x %in% dmns, domains)
            stop('illegal network names: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    if(! missing(site_codes)){
        sits <- unique(macrosheds::ms_site_data$site_code)
        if(any(! site_codes %in% sits)){
            illeg <- Filter(function(x) ! x %in% sits, site_codes)
            stop('illegal network names: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    
    # Fill in missing inputs with NULL
    if(missing(filter_vars)) {
        filter_vars <- NULL
    }
    
    if(grepl('^ws_attr_', prodname)){
        
        if(prodname == 'ws_attr_summaries') msfile <- 'watershed_summaries.feather'
        if(prodname == 'ws_attr_CAMELS_Daymet_forcings') msfile <- 'Daymet_forcings_CAMELS.feather'
        if(prodname == 'ws_attr_CAMELS_summaries') msfile <- 'watershed_summaries_CAMELS.feather'
        
        if(grepl('^ws_attr_timeseries', prodname)){
            attr_set <- stringr::str_extract(prodname, '(?<=ws_attr_timeseries:).*')
            msfile <- paste0('spatial_timeseries_', attr_set, '.feather')
        }
        msfile <- file.path(macrosheds_root, msfile)
        
        # Create size warning
        file_sizes <- file.info(msfile)$size
        file_sizes <- round(sum(file_sizes, na.rm = TRUE)/1000000, 1)
        
        if(warn && file_sizes > 100){
            
            resp <- get_response_1char(msg = paste0('This dataset will occupy about ',
                                                    file_sizes,
                                                    ' MB in memory. Do you want to continue? (y/n) > '),
                                       possible_chars = c('y', 'n'))
            
            if(resp == 'n'){
                message('Aborting dataset load. Sorry, but there\'s no easy way to make this one smaller. Contact us at mail@macrosheds.org')
                return(invisible())
            }
        }
        
        o <- feather::read_feather(msfile)
        
        if(! missing(networks)){
            o <- filter(o, network %in% networks)
        }
        if(! missing(domains)){
            o <- filter(o, domain %in% domains)
        }
        if(! missing(site_codes)){
            o <- filter(o, site_code %in% site_codes)
        }
        
        return(o)
    }
    
    prodname_dirs <- list_all_product_dirs(macrosheds_root = macrosheds_root,
                                           prodname = prodname)
    
    # List network files  
    if(!missing(networks)){
        netdom <- macrosheds::ms_site_data %>%
            select(network, domain) %>%
            distinct(domain, .keep_all = TRUE)
        
        network_domains <- netdom %>%
            filter(network %in% !!networks) %>%
            pull(domain)
        
        # prodpaths_net <- grep(paste0(paste0(macrosheds_root, '/', network_domains, '/', prodname), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        
        prodpaths_net <- grep(paste(network_domains, collapse = '|'), prodname_dirs, value = TRUE)
        prodpaths_net <- list.files(prodpaths_net, full.names = TRUE)
    } else{
        prodpaths_net <- NULL
    }
    
    # List domains files 
    if(!missing(domains)){
        # prodpaths_dom <- grep(paste0(paste0(macrosheds_root, '/', domains, '/', prodname), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        prodpaths_dom <- grep(paste(domains, collapse = '|'), prodname_dirs, value = TRUE)
        prodpaths_dom <- list.files(prodpaths_dom, full.names = TRUE)
    } else{
        prodpaths_dom <- NULL
    }
    
    # List site files 
    all_files <- list.files(prodname_dirs, full.names = T)
    if(!missing(site_codes)){
        sites <- stringr::str_match(string = all_files,
                                    pattern = '([^/]+)(?=\\.feather$)')[,1]
        # prodpaths_sites <- all_files[grep(paste0(site_codes, collapse = '|'), sites)]
        prodpaths_sites <- all_files[sites %in% site_codes]
    } else{
        prodpaths_sites <- NULL
    }
    
    # Load all files 
    if(all(is.null(c(prodpaths_net, prodpaths_dom, prodpaths_sites)))){
        rel_files <- all_files
    } else{
        rel_files <- c(prodpaths_net, prodpaths_dom, prodpaths_sites)
    }
    rel_files <- unique(rel_files)

    # Create size warning
    file_sizes <- file.info(rel_files)$size
    file_sizes <- round(sum(file_sizes, na.rm = TRUE)/1000000, 1)
    
    if(warn && file_sizes > 100){
        
        resp <- get_response_1char(msg = paste0('This dataset will occupy about ',
                                                file_sizes,
                                                ' MB in memory. Do you want to continue? (y/n) > '),
                                   possible_chars = c('y', 'n'))
        
        if(resp == 'n'){
            message('You can reduce dataset size by specifying network, domain, site_code, or filter_vars.')
            return(invisible())
        }
    }
    
    # Read in files 
    d <- purrr::map_dfr(rel_files, feather::read_feather)
    
    if(nrow(d) == 0){
        stop(paste('No results. Check macrosheds_root and verify that desired networks/domains/sites have been',
                   'downloaded (see ms_download_core_data.)'))
    }
        
    if(!is.null(filter_vars)) {
        
        fv_in_d <- filter_vars %in% ms_drop_var_prefix(d$var)
        if(! any(fv_in_d)){
            stop(paste('None of filter_vars is present in this dataset'))
        }
        if(! all(fv_in_d)){
            warning(paste('These variables are not available in this dataset:',
                          filter_vars[! fv_in_d]))
        }
        filter_vars <- filter_vars[fv_in_d]
        
        d <- dplyr::filter(d,
                           ms_drop_var_prefix(var) %in% filter_vars)
    }
    
    if(sort_result){
        d <- arrange(d,
                     site_code, var, datetime)
    }
    
    return(d)
}

