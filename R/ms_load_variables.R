#' load MacroSheds variables data table
#'
#' load a table of MacroSheds variables with information on variable codes,
#' units, and more. (source .Rdata file downloaded with the MacroSheds package)
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

ms_load_variables <- function(expanded = FALSE, verbose = FALSE, type = 'ts'){
  warning('ms_load_variables has time series variable info, type = "ts", AND watershed attribute variable info, type = "ws"')
  tryCatch(
      expr = {
        if(type == 'ts') {
          if(expanded) {
            return(ms_var_catalog)
          } else {
            return(ms_vars_ts)
          }
        } else if(type == 'ws') {
          return(ms_vars_ws)
        } else {
          stop('type not in accepted options: "ts" for time series variable catalog "ws" for watershed attribute variable catalog, "all" for both')
        }
      },
      error = function(e) {
        stop(paste0('failed to load MacroSheds variable catalog into R session'))
      }
    )
}
