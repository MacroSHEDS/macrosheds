#' Download MacroSheds watershed attribute summary data tables
#'
#' Download MacroSheds watershed attribute data for each site, both summary and time series datasets
#' are available
#'
#' @author Wes Slaughter, \email{wslaughter@@berkeley.edu}
#' @author Mike Vlah
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @param ws_attr_dir character. Directory where macrosheds watershed attribute data files will be downloaded.
#'    If this directory does not exist, it will be created.
#' @param dataset character. This function can download the two types of watershed attribute dataset
#'    provided by macrosheds. "summaries" will download a feather file containing watershed attributes
#'    summarized for each macrosheds site over all available data. "time series" will download 6 feather
#'    files containing time series data of watershed attributes, these files are each prefixed "spatial_timeseries":
#'    "vegetation", "terrain", "parentmaterial", "landcover", "hydrology", "climate"
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @return returns nothing.
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples
#' ms_download_ws_attr_summary(ws_attr_dir = 'ms/spatial', dataset = 'time series', quiet = FALSE)

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

    # loop through figshare IDs and download each data product
    for(i in 1:n_downloads) {

        rel_code <- rel_download[i]
        fig_call <- paste0(figshare_base, rel_code)


        # we have to get the 'correct' filename via regex of the http call
        hh <- httr::HEAD(fig_call)
        fig_regex = paste0(rel_code, "/(.*?).feather")
        rel_nm = stringr::str_match(hh$url, fig_regex)[2]
        ws_attr_fp = file.path(ws_attr_dir, paste0(rel_nm, '.feather'))

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
