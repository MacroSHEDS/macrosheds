#' Load MacroSheds site data table
#'
#' Load the MacroSheds site data table with information on networks, domains,
#'  location of stream gauges, and more.
#'
#' @author Wes Slaughter, \email{weston.slaughter@duke.edu}
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Spencer Rhea
#' @return returns a \code{data.frame} of all sites in the MacroSheds dataset
#'    with their corresponding network, domain, latitude, longitude, area, and
#'    additional information.
#' @details Site metadata are included as an .RData file with the macrosheds package, and
#'    are loaded into the macrosheds namespace on library(macrosheds). This
#'    function simply retrieves a data.frame from the macrosheds namespace and makes it available
#'    in the global environment.
#' @export
#' @seealso [ms_load_variables()], [ms_load_spatial_product()], [ms_load_product()]
#' @examples
#' site_data <- ms_load_sites()

ms_load_sites <- function(){

    library("dplyr", quietly = TRUE)

    requireNamespace('macrosheds', quietly = TRUE)
    
    #thisenv <- environment()
    #
    #data(ms_site_data,
    #     package = 'macrosheds',
    #     envir = thisenv,
    #     verbose = FALSE)

    tryCatch(
        expr = {
            return(macrosheds::ms_site_data)
        },
        error = function(e) {
            stop('failed to load macrosheds site data into R session')
        }
    )
}
