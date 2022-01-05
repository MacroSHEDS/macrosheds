#' Load in macrosheds data products
#'
#' Loads and filters macrosheds products (e.g stream_chemistry, discharge, etc.) from a 
#' downloaded macrosheds dataset see \code{download_ms_code_data()} for data download
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, e.g. '~/stuff/macrosheds_dataset_v0.3'
#' @param prodname character. read and combine files associated with this prodname. 
#'    Available prodnames are:
#'    discharge, stream_chemistry, stream_flux_inst, precipitation,
#'    precip_chemistry, precip_flux_inst.
#' @param filter_vars character vector. for products like stream_chemistry that include
#'    multiple variables, this filters to just the ones specified (ignores
#'    variable prefixes). To see a catalog of variables, visit macrosheds.org
#' @param networks character vector. Networks to load, optional
#' @param domains character vector. Domains to load, optional
#' @param site_codes character vector. Sites to load, optional
#' @param sort_result logical. If TRUE, output will be sorted by site_code, var,
#'    datetime. this may take a few additional minutes for some products in
#'    the entire dataset.
#' @param warn logical. If TRUE, function will give a prompt with the estimated 
#'     file size before loading in data.
#' @return returns a \code{tibble} containing all the all data belonging to the 
#'    selected product and variables in the \code{macrosheds_root} directory
#' @export
#' @examples
#' macrosheds_data <- load_product(macrosheds_root = 'data/macrosheds_v1', 
#'                                 prodname = 'stream_chemistry', 
#'                                 sort_result = FALSE, 
#'                                 filter_vars = 'NO3_N')

ms_load_product <- function(macrosheds_root, 
                            prodname,
                            filter_vars,
                            networks,
                            domains,
                            site_codes,
                            sort_result = FALSE,
                            warn = TRUE){
    
    
    # Checks 
    avail_prodnames <- c('discharge', 'stream_chemistry', 'stream_flux_inst_scaled',
                         'precipitation', 'precip_chemistry', 'precip_flux_inst_scaled')

    if(! dir.exists(macrosheds_root)){
        stop('macrosheds_root does not exist, please ensure correct directory is supplied')
    }
    if(missing(macrosheds_root)) { 
        stop('macrosheds_root must be supplied')
        }
    if(missing(prodname)) {
        stop('A prodname must be supplied such as stream_chemistry, discharge, etc.')
    }

    if(! prodname %in% avail_prodnames){
        stop(paste0('prodname must be one of: ',
                    paste(avail_prodnames,
                          collapse = ', ')))
    }
    
    # Fill in missing inputs with NULL
    if(missing(filter_vars)) {
        filter_vars <- NULL
    }
    
    prodname_dirs <- list_all_product_dirs(macrosheds_root = macrosheds_root,
                                          prodname = prodname)
    
    # List network files  
    if(!missing(networks)){
        netdom <- ms_download_site_data() %>%
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
        prodpaths_sites <- all_files[grep(paste0(site_codes, collapse = '|'), sites)]
    } else{
        prodpaths_sites <- NULL
    }
    
    rel_files <- c(prodpaths_net, prodpaths_dom, prodpaths_sites)
    rel_files <- unique(rel_files)

    # Create size warning
    file_sizes <- file.info(rel_files)$size
    file_sizes <- round(sum(file_sizes, na.rm = TRUE)/1000000, 1)
    
    if(warn){
        
        continue <- readline(paste0('These files could take up to ', file_sizes, 'MB. Do you want to load these files (Y/N)? '))
        if(!continue %in% c('y', 'Y', 'n', 'N')){
            continue <- readline('Please enter Y or N ')
        }
        
        if(continue == 'N' || continue == 'n'){
            stop('If you are worried about the size of these files, try refining the amount files you are reading in by supplying the domain or filter_vars')
        }
    }
    
    # Read in files 
    d <- purrr::map_dfr(rel_files, feather::read_feather)
        
    if(!is.null(filter_vars)) {
        d <- dplyr::filter(d,
                           ms_drop_var_prefix(var) %in% filter_vars)
    }
    
    # If no results are returned generate message 
    if(nrow(d) == 0){
        if(missing(filter_vars)){
            stop('No results. Make sure macrosheds_root is correct.')
        } else {
            stop(paste('No results. Make sure macrosheds_root is correct and',
                       'filter_vars includes variable codes from the catalog',
                       'on macrosheds.org'))
        }
    }
    
    if(sort_result){
        d <- arrange(d,
                     site_code, var, datetime)
    }
    
    return(d)
}

