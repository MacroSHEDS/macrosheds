#' Download macrosheds core datasets
#'
#' Download the macrosheds core datasets of stream chemistry, stream 
#' discharge, stream chemical flux, precipitation, precipitation chemistry, and
#' precipitation chemical flux. Not all products are available at all sites but 
#' all products available for the selected domains will be downloaded.
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character.  Directory where macrosheds data files will be downloaded to
#' @param networks character vector. macrosheds networks that will be downloaded. 
#'    Either a single network, vector of networks, or 'all'. see \code{download_ms_site_data()} 
#'    for networks available for download.
#' @param domains character vector. macrosheds domains that will be downloaded. 
#'    Either a single domain, vector of domains, or 'all'. see \code{download_ms_site_data()} 
#'    for domains available for download.
#' @param quiet logical. If TRUE, no download messages will be printed in console
#' @return Downloads all core data for selected domains to the designated 
#'    directory listed in \code{macrosheds_root}. Data follows the structure: domain/product/site_code.feather. 
#'    Where domain is the domain selected, product is one or more of the data 
#'    product available at a domain (eg. stream_chemistry__ms001, discharge__ms002), 
#'    and site_code is the code for an individual site. See \code{download_ms_site_data()} 
#'    for more information on sites. 
#' 
#' @details Only \code{networks} or \code{domains} must be supplied. If 'all' is 
#'    supplied to either argument, all domains will be downloaded regardless of 
#'    what is supplied to the other argument. Downloading all macrosheds core data
#'    can take up to ~5 GB, so beware of downloading too much data.
#' @export
#' @examples
#' dir.create('data/macrosheds')
#' download_ms_core_data(macrosheds_root = 'data/macrosheds',
#'                       domains = c('niwot', 'hjandrews'))

ms_download_core_data <- function(macrosheds_root,
                                  networks,
                                  domains,
                                  quiet = FALSE) {
    
    dom_missing <- missing(domains)
    net_missing <- missing(networks)
    
    if(dom_missing && net_missing) {
        return('At least one domain or network must be listed. Networks and domains can be found in the site data file download_ms_site_data()')
    }
    
    if(missing(macrosheds_root)) {
        return('The directory to where the files will be saved must be supplied')
    }
    
    figshare_base <- 'https://figshare.com/ndownloader/files/'
    
    figshare_codes <- data.frame(network = c('krycklan', 'walker_branch', 'usgs', 'usfs', 'usfs',
                                             'usfs', 'usfs', 'neon', 'lter', 'lter', 'lter', 'lter',
                                             'lter', 'lter', 'lter', 'lter', 'lter', 'lter', 'lter',
                                             'doe', 'czo', 'czo', 'czo', 'czo'),
                                 domain = c('krycklan', 'walker_branch', 'usgs', 'suef', 'santee',
                                            'krew', 'fernow', 'neon', 'santa_barbara', 'plum', 'niwot',
                                            'mcmurdo', 'luquillo', 'konza', 'hjandrews', 'hbef', 'bonanza',
                                            'baltimore', 'arctic', 'east_river', 'shale_hills',
                                            'catalina_jemez', 'calhoun', 'boulder'),
                                 fig_code = c(30829432, 30829456, 30829453, 30829441, 30829432,
                                              30829426, 30829408, 30829393, 30829387, 30829375,
                                              30829372, 30829369, 30829333, 30829318, 30829240,
                                              30828991, 30828985, 30828979, 30828922, 30827524,
                                              30827185, 30824872, 30824785, 30824707))
    
    if(net_missing){
        networks <- 'none'
    }
    
    if(dom_missing){
        domains <- 'none'
    }
    
    if(domains == 'all' || networks == 'all') {
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
    
    for(i in 1:nrow(rel_download)) {
        
        temp_dir <- tempdir()
        rel_dom <- rel_download[i,2]
        rel_code <- rel_download[i,3]
        temp_file_dom <- paste0(temp_dir, '/', rel_dom, '.zip')
        
        fig_call <- paste0(figshare_base, '/', rel_code)
        
        if(quiet){
            download_status <- try(download.file(url = fig_call,
                                                 destfile = temp_file_dom,
                                                 quiet = TRUE),
                                   silent = TRUE)
            
            if(inherits(download_status, 'try-error')){
                print(paste0(rel_dom, ' failed to download.'))
                next
            }
        } else{
            download_status <- sm(try(download.file(url = fig_call,
                                                 destfile = temp_file_dom),
                                   silent = TRUE))
            
            if(inherits(download_status, 'try-error')){
                next
            }
        }
        
        
        unzip_status <- try(unzip(zipfile = temp_file_dom,
                                  exdir = macrosheds_root),
                            silent = TRUE)
        
        if(quiet){
            if(inherits(unzip_status, 'try-error')){
                next
            }
        } else{
            if(inherits(unzip_status, 'try-error')){
                print(paste0(rel_dom, ' failed to unzip.'))
                next
            }
            
            print(paste0(rel_dom, ' successfully downloaded and unziped.'))
        }
    }
    
    if(quiet){
        return()
    } else{
        return('Download complete')
    }
}


