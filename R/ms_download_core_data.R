#' Download macrosheds core datasets
#'
#' Download the macrosheds core datasets of stream chemistry, stream
#' discharge, stream solute flux, precipitation, precipitation chemistry,
#' precipitation solute flux, annual solute loads, ws_boundary, stream_gauge_locations, and
#' precip_gauge_locations. Not all products are available at all sites, but
#' all products available for the selected domains will be downloaded.
#'
#' @author Spencer Rhea
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param macrosheds_root character. Directory where macrosheds data files will be downloaded.
#'    If this directory does not exist, it will be created. The `macrosheds_root` directory should
#'    only be used for MacroSheds data files.
#' @param networks character vector. macrosheds networks that will be downloaded.
#'    Either a single network, vector of networks, or 'all'. Either `networks` or `domains` must be supplied.
#'    See [ms_load_sites()] for networks available for download.
#' @param domains character vector. macrosheds domains that will be downloaded.
#'    Either a single domain, vector of domains, or 'all'. Either `domains` or `networks` must be supplied.
#'    See [ms_load_sites()] for domains available for download.
#' @param version numeric or "latest". The MacroSheds dataset version to download.
#' @param skip_existing logical. If FALSE, dataset components already downloaded to macrosheds_root will
#'    be overwritten. If TRUE, these will be skipped.
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @return Downloads all core data for selected domains to the
#'    directory specified by `macrosheds_root`. Site datasets are arranged according to the following
#'    structure: domain/prodname/site_code.feather. For definitions of these terms as used by
#'    MacroSheds, see glossary.txt on [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262).
#' @details The core time-series dataset is approximately 6 GiB uncompressed.
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
                                  skip_existing = TRUE,
                                  quiet = FALSE){

    library("dplyr", quietly = TRUE)

    requireNamespace('macrosheds', quietly = TRUE)

    dom_missing <- missing(domains)
    net_missing <- missing(networks)

    if(dom_missing && net_missing){
        stop('At least one domain or network must be specified. Run ms_load_sites() for a catalog of networks and domains.')
    }

    if(missing(macrosheds_root)){
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

    if(version == 'latest'){
        version <- max(avail_vsns)
        cat('Retrieving data from MacroSheds v', version, '\n')
    }

    figshare_codes <- figshare_codes %>%
        select(network, domain, fig_code = !!paste0('fig_code_v', version))

    if(net_missing) networks <- NULL
    if(dom_missing) domains <- NULL

    sited <- macrosheds::ms_load_sites()

    ntw_in <- networks %in% c(unique(sited$network), 'all')
    if(any(! ntw_in)){
        stop('Not MacroSheds network(s): ',
             paste(networks[! ntw_in], collapse = ', '),
             '.\nSee ms_load_sites()')
    }

    dmn_in <- domains %in% c(unique(sited$domain), 'all')
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

    macrosheds_root <- normalizePath(macrosheds_root)

    if(! dir.exists(macrosheds_root)){
        print(paste0('Creating macrosheds_root at ',
            macrosheds_root))
        dir.create(macrosheds_root, recursive = TRUE)
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

    root_vsn <- file.path(macrosheds_root, paste0('v', version))

    if(skip_existing){
        existing_dirs <- list.files(root_vsn)
        ovw_doms <- intersect(rel_download$domain, existing_dirs)
        if(length(ovw_doms)){
            cat(paste0('Data for these domains already present in', root_vsn, ':\n\t',
                       paste(ovw_doms, collapse = ', '),
                       '\n\tSet skip_existing = FALSE to overwrite. For now, skipping these domains.\n'))
            rel_download <- filter(rel_download, ! domain %in% ovw_doms)
        }
    }

    n_downloads <- nrow(rel_download)

    if(n_downloads == 0){
        cat('Nothing to do\n')
        return(invisible())
    }

    temp_dir <- tempdir()
    for(i in seq_len(n_downloads)){

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

        unzip_status <- try(unzip(zipfile = temp_file_dom, exdir = root_vsn))

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
