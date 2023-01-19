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

ms_download_ws_attr_summary <- function(macrosheds_root){
    if(missing(macrosheds_root)) {
        return('The directory to where the files will be saved must be supplied')
    }

    if(!exists('file_ids_for_r_package2')) {
      stop('necessary data files missing, should be pre-loaded with MacroSheds package download',
           'user may need to re-install macrosheds R package to use this function')
    }

    figshare_base <- 'https://figshare.com/ndownloader/files/'
    figshare_codes <- file_ids_for_r_package2 #loaded in R/sysdata2.rda

    if(!dir.exists(macrosheds_root)) {
        print('Creating macrosheds_root becuase it does not currently exist')
        dir.create(macrosheds_root, recursive = TRUE)
    }



    tryCatch(
      expr = {
        ms_sites <- download.file('')
      },
      error = function(e) {
        stop('failed to load macrosheds site data into R session')
      }
      )
}
