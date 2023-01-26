#' Download MacroSheds watershed attributes
#'
#' Download MacroSheds watershed attribute data for all sites. Choose between
#' temporally explicit spatial summaries, or summaries across both time and space.
#'
#' @author Wes Slaughter, \email{wslaughter@@berkeley.edu}
#' @author Mike Vlah
#' @author Spencer Rhea
#' @param macrosheds_root character. Directory where watershed attribute files will be downloaded.
#'    If this directory does not exist, it will be created. Does not have to be the same
#'    as \code(macrosheds_root) provided to [ms_download_core_data()], but might as well be.
#' @param dataset character. This function can download the two types of watershed attribute data
#'    provided by MacroSheds. "summaries" will download a feather file containing watershed attributes
#'    summarized across time and space (i.e. one value for each site). "time series" will download 6 feather
#'    files containing time series of watershed attributes where available. Data from these
#'    files is loaded with [ms_load_ws_attr()].
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @param omit_climate_data logical. Ignored if \code{dataset == 'summaries'}. However, if
#'    \code{dataset == 'time series'}, and you don't care about climate data,
#'    you may use this argument to avoid downloading it (because it's huge), while still downloading
#'    terrain, vegetation, parent material, land use, and hydrology data (which are tiny).
#' @return returns NULL. Writes files to disk.
#' @export
#' @seealso [ms_download_core_data()], [ms_load_ws_attr()]
#' @examples
#' ms_download_ws_attr(macrosheds_root = 'my/macrosheds/root', dataset = 'time series')

ms_download_ws_attr <- function(macrosheds_root, dataset = 'summaries', quiet = FALSE,
                                omit_climate_data = FALSE){
    
    requireNamespace('macrosheds', quietly = TRUE)
    library('dplyr', quietly = TRUE)

    # figshare basic info handling
    if(inherits(try(macrosheds::file_ids_for_r_package2, silent = TRUE), 'try-error')){
      stop('Necessary data files missing; should be pre-loaded with MacroSheds package download. ',
           'User may need to re-install macrosheds to use this function.')
    }
    
    if(! dataset %in% c('summaries', 'time series')){
        stop('dataset must be either "summaries" or "time series". See help files.')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    figshare_codes <- macrosheds::file_ids_for_r_package2 #loaded in R/sysdata2.rda

    # download directory handling
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied')
    }

    if(!dir.exists(macrosheds_root)) {
        print('Creating macrosheds_root becuase it does not currently exist')
        dir.create(macrosheds_root, recursive = TRUE)
    }

    # choose fighsare id for chosen dataset
    if(dataset == 'summaries') {
        
        rel_download <- figshare_codes %>%
            filter(ut == 'watershed_summaries')
        
    } else if(dataset == 'time series') {
        
        rel_download <- figshare_codes %>%
            filter(grepl('timeseries', ut))
      
        if(omit_climate_data){
            if(! quiet) print('omitting climate data from download')
            rel_download <- rel_download[-1, ]
        }
    }

    n_downloads <- nrow(rel_download)
    if(! n_downloads) stop('Could not find remote file. Try reinstalling macrosheds.')

    # loop through figshare IDs and download each data product
    for(i in 1:n_downloads) {

        rel_code <- rel_download$fig_code[i]
        filename <- rel_download$ut[i]
        fig_call <- paste0(figshare_base, rel_code)
        ws_attr_fp <- file.path(macrosheds_root, paste0(filename, '.feather'))

        if(!quiet){
            print(glue::glue('Downloading dataset type: {ds} ({ii}/{iN}; Figshare code {rc})',
                             ds = dataset,
                             ii = i,
                             iN = n_downloads,
                             rc = rel_code))
        }

        download.file(url = fig_call,
                      destfile = ws_attr_fp,
                      quiet = quiet,
                      cacheOK = FALSE,
                      mode = 'wb')

        if(! quiet) print(glue::glue('{filename}.feather successfully downloaded to {macrosheds_root}'))
    }

    return(invisible())
}
