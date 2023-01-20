
#' Load MacroSheds site data table
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
#' ms_site_data <- ms_load_sites()

ms_load_sites <- function(){
  tryCatch(
    expr = {
      return(ms_site_data)
    },
    error = function(e) {
      stop('failed to load macrosheds site data into R session')
    }
    )
}
