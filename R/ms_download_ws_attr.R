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
#'    as \code{macrosheds_root} provided to [ms_download_core_data()], but might as well be.
#' @param dataset character. This function can download each of the four collections of watershed attribute data
#'    provided by MacroSheds.
#' * "summaries" will download watershed attributes summarized across time and space (i.e. one value for each site).
#' * "time series" will download temporally explicit watershed attributes where available. See \code{omit_climate_data} parameter. 
#' * "CAMELS summaries" and...
#' * "CAMELS Daymet forcings" will download additional watershed
#'   summary data that conform as closely as possible to the specifications of the
#'   [CAMELS dataset](https://ral.ucar.edu/solutions/products/camels). See MacroSheds metadata
#'   for a list of discrepancies.
#' * you may also use "all" to retrieve all four datasets. \code{omit_climate_data} will still be recognized.
#'
#' Once downloaded, data can be loaded into R with [ms_load_product()].
#' @param version character. The MacroSheds dataset version to download, e.g. "1.0". Defaults to 
#'    most recent. As of 2023-03-17, only version 1.0 is available, so this parameter is a stub.
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @param omit_climate_data logical. Ignored unless \code{dataset == 'time series'}. If you don't care about climate data,
#'    you may use this argument to avoid downloading it (because it's huge), while still downloading
#'    terrain, vegetation, parent material, land use, and hydrology data (which are tiny).
#' @param timeout integer. Temporarily overrides getOption(timeout).
#' @return Returns NULL. Downloads watershed attribute data to the
#'    directory specified by \code{macrosheds_root}. For documentation, visit
#'   [EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262). 
#' @seealso [ms_download_core_data()], [ms_load_product()]
#' @export
#' @examples
#' ms_download_ws_attr(macrosheds_root = 'my/macrosheds/root', dataset = 'time series')

ms_download_ws_attr <- function(macrosheds_root, dataset = 'summaries', quiet = FALSE,
                                version = "1.0", omit_climate_data = FALSE,
                                timeout = 10000){
    
    requireNamespace('macrosheds', quietly = TRUE)
    library('dplyr', quietly = TRUE)

    # figshare basic info handling
    if(inherits(try(macrosheds::file_ids_for_r_package2, silent = TRUE), 'try-error')){
      stop('Necessary data files missing; should be pre-loaded with MacroSheds package download. ',
           'User may need to re-install macrosheds to use this function.')
    }
    
    if(! dataset %in% c('summaries', 'time series', 'CAMELS summaries', 'CAMELS Daymet forcings', 'all')){
        stop('dataset must be one of "summaries", "time series", "CAMELS summaries", "CAMELS Daymet forcings", "all". See help files.')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    figshare_codes <- macrosheds::file_ids_for_r_package2 #loaded in R/sysdata2.rda

    # download directory handling
    if(missing(macrosheds_root)) {
        stop('macrosheds_root must be supplied')
    }

    if(! dir.exists(macrosheds_root)) {
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
        
    } else if(dataset == 'CAMELS summaries') {
        
        rel_download <- figshare_codes %>%
            filter(grepl('watershed_summaries_CAMELS', ut))
        
    } else if(dataset == 'CAMELS Daymet forcings') {
        
        rel_download <- figshare_codes %>%
            filter(grepl('Daymet_forcings_CAMELS', ut))
    } else if(dataset == 'all') {

        rel_download <- figshare_codes %>%
            filter(! grepl('README|POLICY', ut))

        if(omit_climate_data){
            if(! quiet) print('omitting temporally explicit climate data from download')
            rel_download <- filter(rel_download, ut != 'spatial_timeseries_climate')
        }
    }

    n_downloads <- nrow(rel_download)
    if(! n_downloads) stop('Could not find remote file. Reinstall macrosheds to update remote links.')

    # save user default timeout value and set new
    default_timeout <- getOption('timeout')
    options(timeout = timeout)

    # loop through figshare IDs and download each data product
    for(i in 1:n_downloads) {

        rel_code <- rel_download$fig_code[i]
        filename <- rel_download$ut[i]
        fig_call <- paste0(figshare_base, rel_code)
        ws_attr_fp <- file.path(macrosheds_root, paste0(filename, '.feather'))

        if(!quiet){
            print(glue::glue('Downloading dataset type: {ds} ({ii}/{iN}; download id {rc})',
                             ds = dataset,
                             ii = i,
                             iN = n_downloads,
                             rc = rel_code))
        }

        dl <- try(download.file(url = fig_call,
                  destfile = ws_attr_fp,
                  quiet = quiet,
                  cacheOK = FALSE,
                  mode = 'wb'))
    
        options(timeout = default_timeout)

        if(! quiet) print(glue::glue('{filename}.feather successfully downloaded to {macrosheds_root}'))
    }

    return(invisible())
}
