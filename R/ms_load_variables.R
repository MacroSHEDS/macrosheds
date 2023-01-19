#' load macrosheds variables data table
#'
#' load a table of MacroSheds variables with information on variable codes,
#' units, and more from a .Rdata file downloaded with the macrosheds package.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param expanded logical. If TRUE, ms_load_variables will load in the complete variable sheet which includes dozens of
#'     more obscure molecules.
#' @return returns a \code{data.frame} of variables, codes,
#'     and units in MacroSheds format
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples
#' ms_vars <- ms_download_variables()

ms_load_variables <- function(expanded = TRUE, verbose = TRUE){

  tryCatch(
    expr = {
      ms_vars <- load('./data/ms_var_catalog.RData', verbose = verbose)

    },
    error = function(e) {
      writeLines(paste0('failed to load macrosheds variable catalog into R session'))
    }
    )



    return(ms_vars)
}
