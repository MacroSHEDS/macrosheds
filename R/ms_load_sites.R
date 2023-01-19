
#' load macrosheds site data
#'
#' Load in MacroSheds site information
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of site data
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples

ms_load_sites <- function(verbose = FALSE){
  tryCatch(
    expr = {
      load('./data/', verbose = verbose)
    },
    error = function(e) {
      stop(paste0('failed to load macrosheds site data into R session'))
    }
    )

    return(ms_sites)
}
