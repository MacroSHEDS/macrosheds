#' Download macrosheds core datasets
#'
#' Download the macrosheds core datasets of stream chemistry, stream 
#' discharge, stream chemical flux, precipitation, precipitation chemistry,
#' precipitation chemical flux, ws_boundary, stream_gauge_locations, and
#' precip_gauge_locations. Not all products are available at all sites, but 
#' all products available for the selected domains will be downloaded.
#'
#' @author Spencer Rhea 
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param macrosheds_root character. Directory where macrosheds data files will be downloaded.
#'    If this directory does not exist, it will be created.
#' @param networks character vector. macrosheds networks that will be downloaded. 
#'    Either a single network, vector of networks, or 'all'. See \code{ms_load_sites()} 
#'    for networks available for download.
#' @param domains character vector. macrosheds domains that will be downloaded. 
#'    Either a single domain, vector of domains, or 'all'. See \code{ms_load_sites()} 
#'    for domains available for download.
#' @param version numeric or "latest". The MacroSheds dataset version to download.
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @return Downloads all core data for selected domains to the
#'    directory specified by \code{macrosheds_root}. Site datasets are arranged according to the following
#'    structure: domain/prodname/site_code.feather. For definitions of these terms as used by
#'    MacroSheds, see glossary.txt on [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262). 
#' 
#' @details Either \code{networks} or \code{domains} must be supplied. If 'all' is 
#'    supplied to either argument, all domains will be downloaded regardless of 
#'    what is supplied to the other argument. The full (core) dataset is approximately 500 MiB when
#'    compressed and may take minutes to hours to download, depending on connection speed. When uncompressed,
#'    the dataset is approximately 4 GiB, so be mindful of disk space.
#' @export
#' @seealso [ms_load_sites()], [ms_load_variables()], [ms_load_product()], [ms_load_spatial_product()]
#' @examples
#' dir.create('data/macrosheds', recursive = TRUE)
#' ms_download_core_data(macrosheds_root = 'data/macrosheds',
#'                       domains = c('niwot', 'hjandrews'))

ms_download_core_data <- function(macrosheds_root,
                                  networks,
                                  domains,
                                  version = "latest",
                                  quiet = FALSE){

    library("dplyr", quietly = TRUE)
    
    requireNamespace('macrosheds', quietly = TRUE)

    dom_missing <- missing(domains)
    net_missing <- missing(networks)
    
    if(dom_missing && net_missing) {
        stop('At least one domain or network must be specified. Networks and domains can be retrieved with ms_load_sites()')
    }
    
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied.')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    figshare_codes <- macrosheds::file_ids_for_r_package #loaded in R/sysdata.rda, which is written in postprocessing
    
    version <- as.character(version)
    
    avail_vsns <- figshare_codes %>% 
        select(starts_with('fig_code_')) %>% 
        rename_with(~sub('fig_code_v', '', .)) %>% 
        colnames()
    
    if(! version %in% c(avail_vsns, 'latest')){
        stop('Unknown `version`. Available versions are: ',
             paste(avail_vsns, collapse = ', '),
             '. Or use "latest".')
    }
    
    avail_vsns <- as.numeric(avail_vsns)
    
    if(version == 'latest') version <- max(avail_vsns)
    
    figshare_codes <- figshare_codes %>% 
        select(network, domain, fig_code = !!paste0('fig_code_v', version))
    
    if(net_missing){
        networks <- 'none'
    }
    
    if(dom_missing){
        domains <- 'none'
    }

    sited <- macrosheds::ms_load_sites()
    
    ntw_in <- networks %in% c(unique(sited$network), 'all', 'none')
    if(any(! ntw_in)){
        stop('Not MacroSheds network(s): ',
             paste(networks[! ntw_in], collapse = ', '),
             '.\nSee ms_load_sites()')
    }
    
    dmn_in <- domains %in% c(unique(sited$domain), 'all', 'none')
    if(any(! dmn_in)){
        stop('Not MacroSheds domain(s): ',
             paste(domains[! dmn_in], collapse = ', '),
             '.\nSee ms_load_sites()')
    }
    
    if((length(domains) == 1 && domains == 'all') ||
       (length(networks) == 1 && networks == 'all')){
        rel_download <- figshare_codes
    } else {
        if(! dom_missing && ! net_missing){
            rel_download <- figshare_codes %>%
                dplyr::filter(domain %in% !!domains | network %in% !!networks)
        } else {
            if(net_missing){
                rel_download <- figshare_codes %>%
                    dplyr::filter(domain %in% !!domains)
            }
            if(dom_missing){
                rel_download <- figshare_codes %>%
                    dplyr::filter(network %in% !!networks)
            }
        }
    }
    
    unavail_vsn <- is.na(rel_download$fig_code)
    if(any(unavail_vsn)){
        
        unavail_ntw_dmn <- rel_download %>% 
            filter(unavail_vsn) %>% 
            mutate(ntw_dmn = paste(network, domain, sep = '-')) %>% 
            pull(ntw_dmn)
        
        stop('The requested version (', version,
             ') is not available for the following network-domains:\n\t',
             paste(sort(unavail_ntw_dmn), collapse = ', '))
    }
    
    if(! dir.exists(macrosheds_root)){
        print(paste0('Creating macrosheds_root at ',
            file.path(getwd(), macrosheds_root)))
        dir.create(macrosheds_root, recursive = TRUE)
    }
    
    n_downloads <- nrow(rel_download)
    
    for(i in seq_len(n_downloads)){
        
        temp_dir <- tempdir()
        rel_dom <- rel_download$domain[i]
        rel_code <- rel_download$fig_code[i]
        temp_file_dom <- paste0(temp_dir, '/', rel_dom, '.zip')
        
        fig_call <- paste0(figshare_base, rel_code)
        
        if(! quiet){
            print(glue::glue('Downloading domain: {rd} ({ii}/{iN}; download id {rc})',
                             rd = rel_dom,
                             ii = i,
                             iN = n_downloads,
                             rc = rel_code))
        }
        
        download_status <- try(download.file(url = fig_call,
                                             destfile = temp_file_dom,
                                             quiet = quiet,
                                             cacheOK = FALSE,
                                             mode = 'wb'))
        
        fails <- c()
        if(inherits(download_status, 'try-error')){
            fails <- c(fails, pull(rel_dom))
            next
        }
        
        unzip_status <- try(unzip(zipfile = temp_file_dom,
                                  exdir = macrosheds_root))
        
        if(inherits(unzip_status, 'try-error')){
            fails <- c(fails, pull(rel_dom))
            next
        }
        
        if(! quiet) print(paste(rel_dom, 'successfully downloaded and unzipped.'))
    }
    
    if(length(fails)){
        report <- paste0('These domains failed to download: ',
                        paste(fails, collapse = ', '),
                        '. Do you need to increase timeout limit with e.g. `options(timeout = 3600)`?')
        warning(report)
    } else {
        cat('All downloads succeeded\n')
    }
}


