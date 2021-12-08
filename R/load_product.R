#' Load in macrosheds data products
#'
#' Loads an entire macrosheds product (e.g stream_chemistry, discharge, etc.) from a 
#' downloaded macrosheds dataset see \code{download_ms_code_data()} for data download
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the macrosheds dataset's parent
#'    directory, e.g. '~/stuff/macrosheds_dataset_v0.3'
#' @param prodname character. read and combine all files associated with this prodname
#'    across all networks and domains. Available prodnames are:
#'    discharge, stream_chemistry, stream_flux_inst, precipitation,
#'    precip_chemistry, precip_flux_inst.
#' @param filter_vars character vector. for products like stream_chemistry that include
#'    multiple variables, this filters to just the ones specified (ignores
#'    variable prefixes). To see a catalog of variables, visit macrosheds.org
#' @param domains character vector. Domains to load, optional
#' @param sort_result logical. If TRUE, output will be sorted by site_code, var,
#    datetime. this may take a few additional minutes for some products in
#    the full 15m dataset.
#' @return returns a \code{tibble} containing all the all data belonging to the 
#'    selected data product and variables in the \code{macrosheds_root} directory
#' @export
#' @examples
#' macrosheds_data <- load_product(macrosheds_root = 'data/macrosheds_v1', 
#'                                 prodname = 'stream_chemistry', 
#'                                 sort_result = FALSE, 
#'                                 filter_vars = 'NO3_N')

load_product <- function(macrosheds_root,
                         prodname,
                         filter_vars,
                         domains,
                         sort_result = FALSE){
    
    # Checks 
    avail_prodnames <- c('discharge', 'stream_chemistry', 'stream_flux_inst_scaled',
                         'precipitation', 'precip_chemistry', 'precip_flux_inst_scaled')

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
    
    if(missing(filter_vars)) {
        filter_vars <- NULL
    }
    if(missing(domains)) {
        domains <- NULL
    }
    
    prodname_dirs <- list_all_product_dirs(macrosheds_root = macrosheds_root,
                                          prodname = prodname)
    
    d <- tibble()
    for(pd in prodname_dirs){
        
        rgx <- paste0('(?<=', macrosheds_root, ').+__ms[0-9]{3}')
        domain_prod <- str_extract(string = pd,
                                   pattern = rgx)
        
        if(substr(domain_prod, 0,1) == '/') {
            domain_prod <- substr(domain_prod, 2, nchar(domain_prod))
        }
        
        domain <- str_split_fixed(domain_prod, '/', n = Inf)[1]
        
        if(!is.null(domains)) {
            if(!domain %in% domains) next
        }
        
        d0 <- list.files(pd, full.names = TRUE) %>%
            purrr::map_dfr(feather::read_feather)
        
        if(!is.null(filter_vars)) {
            d0 <- dplyr::filter(d0,
                                drop_var_prefix(var) %in% filter_vars)
        }
        
        d <- d0 %>%
            mutate(domain = !!domain) %>%
            bind_rows(d)
    }
    
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

