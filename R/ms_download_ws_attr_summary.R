#' Download MacroSheds watershed attribute summary data tables
#'
#' Download the MacroSheds site data table with information on networks, domains,
#'  location of stream gauges, and more. (source .Rdata file downloaded with the MacroSheds package)
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of all sites in the MacroSheds systems
#'    with their corresponding network, domain, latitude, longitude, area, and
#'    additional information.
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples
#' ms_site_data <- ms_download_ws_attr_summary()

ms_download_ws_attr_summary <- function(ws_attr_dir, dataset = 'summaries', quiet = FALSE){
    requireNamespace('macrosheds', quietly = TRUE)

    # figshare basic info handling
    if(!exists('file_ids_for_r_package2')) {
      stop('necessary data files missing, should be pre-loaded with MacroSheds package download',
           'user may need to re-install macrosheds R package to use this function')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    figshare_codes <- file_ids_for_r_package2 #loaded in R/sysdata2.rda

    # download directory handling
    if(missing(ws_attr_dir)) {
        return('The directory to where the files will be saved must be supplied')
    }

    if(!dir.exists(ws_attr_dir)) {
        print('Creating ws_attr_dir becuase it does not currently exist')
        dir.create(ws_attr_dir, recursive = TRUE)
    }

    # choose fighsare id for chosen dataset
    if(dataset == 'summaries') {
      rel_download <- figshare_codes %>%
        filter(ut == 'watershed_summaries') %>%
        pull(fig_code)
    } else if(dataset == 'time series') {
      rel_download <- figshare_codes %>%
        filter(grepl('timeseries', ut)) %>%
        pull(fig_code)
    } else {
      return('dataset argument must be either "time series" or "summaries"')
    }

    n_downloads <- length(rel_download)

    for(i in 1:n_downloads) {

        rel_code <- rel_download[i]
        rel_nm = paste0(dataset, rel_code)
        ws_attr_fp = paste0(ws_attr_dir, '/', rel_nm, '.feather')

        fig_call <- paste0(figshare_base, '_', rel_code)

        if(! quiet){
            print(glue::glue('Downloading dataset type: {ds} ({ii}/{iN}; Figshare code {rc})',
                             ds = dataset,
                             ii = i,
                             iN = n_downloads,
                             rc = rel_code))
        }

        download_status <- try(download.file(url = fig_call,
                                             destfile = ws_attr_fp,
                                             quiet = quiet,
                                             cacheOK = FALSE,
                                             mode = 'wb'))

        if(inherits(download_status, 'try-error')) next

        if(! quiet) print(paste(rel_nm, 'successfully downloaded and unzipped.'))
    }

    return(invisible())
}
