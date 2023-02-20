#' Download macrosheds core datasets
#'
#' Download the macrosheds core datasets of stream chemistry, stream 
#' discharge, stream chemical flux, precipitation, precipitation chemistry,
#' precipitation chemical flux, ws_boundary, stream_gauge_locations, and
#' precip_gauge_locations. Not all products are available at all sites, but 
#' all products available for the selected domains will be downloaded.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. Directory where macrosheds data files will be downloaded.
#'    If this directory does not exist, it will be created.
#' @param networks character vector. macrosheds networks that will be downloaded. 
#'    Either a single network, vector of networks, or 'all'. See \code{ms_downloadsite_data()} 
#'    for networks available for download.
#' @param domains character vector. macrosheds domains that will be downloaded. 
#'    Either a single domain, vector of domains, or 'all'. See \code{ms_downloadsite_data()} 
#'    for domains available for download.
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @return Downloads all core data for selected domains to the
#'    directory specified by \code{macrosheds_root}. Site datasets are arranged according to the following
#'    structure: domain/prodname/site_code.feather. For definitions of these terms as used by
#'    MacroSheds, see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262). 
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
                                  quiet = FALSE) {

    library("dplyr", quietly = TRUE)
    
    requireNamespace('macrosheds', quietly = TRUE)

    dom_missing <- missing(domains)
    net_missing <- missing(networks)
    
    if(dom_missing && net_missing) {
        return('At least one domain or network must be listed. Networks and domains can be found in the site data file ms_downloadsite_data()')
    }
    
    if(missing(macrosheds_root)) {
        return('The directory to where the files will be saved must be supplied')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    
    figshare_codes <- macrosheds::file_ids_for_r_package #loaded in R/sysdata.rda, which is written in postprocessing
    #figshare_codes <- data.frame(network = c('krycklan', 'walker_branch', 'usgs', 'usfs', 'usfs',
    #                                         'usfs', 'usfs', 'neon', 'lter', 'lter', 'lter', 'lter',
    #                                         'lter', 'lter', 'lter', 'lter', 'lter', 'lter', 'lter',
    #                                         'doe', 'czo', 'czo', 'czo', 'czo'),
    #                             domain = c('krycklan', 'walker_branch', 'usgs', 'suef', 'santee',
    #                                        'krew', 'fernow', 'neon', 'santa_barbara', 'plum', 'niwot',
    #                                        'mcmurdo', 'luquillo', 'konza', 'hjandrews', 'hbef', 'bonanza',
    #                                        'baltimore', 'arctic', 'east_river', 'shale_hills',
    #                                        'catalina_jemez', 'calhoun', 'boulder'),
    #                             fig_code = c(30829555, 30829456, 30829453, 30829441, 30829432,
    #                                          30829426, 30829408, 30829393, 30829387, 30829375,
    #                                          30829372, 30829369, 30829333, 30829318, 30829240,
    #                                          30828991, 30828985, 30828979, 30828922, 30827524,
    #                                          30827185, 30824872, 30824785, 30824707))
    
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
       (length(networks) == 1 && networks == 'all')) {
        rel_download <- figshare_codes
    } else{
        if(!dom_missing && !net_missing){
            rel_download <- figshare_codes %>%
                dplyr::filter(domain %in% !!domains | network %in% !!networks)
        } else{
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
    
    if(!dir.exists(macrosheds_root)) {
        print('Creating macrosheds_root becuase it does not currently exist')
        dir.create(macrosheds_root, recursive = TRUE)
    }
    
    n_downloads <- nrow(rel_download)
    
    for(i in 1:n_downloads) {
        
        temp_dir <- tempdir()
        rel_dom <- rel_download[i,2]
        rel_code <- rel_download[i,3]
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
        
        if(inherits(download_status, 'try-error')) next
        
        unzip_status <- try(unzip(zipfile = temp_file_dom,
                                  exdir = macrosheds_root))
        
        if(inherits(unzip_status, 'try-error')) next
        
        if(! quiet) print(paste(rel_dom, 'successfully downloaded and unzipped.'))
    }
    
    return(invisible())
}


