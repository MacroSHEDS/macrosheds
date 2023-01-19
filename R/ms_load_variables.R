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

ms_load_variables <- function(expanded = TRUE, verbose = FALSE, type = 'all'){

  tryCatch(
    expr = {
      if(type == 'all') {
        load('./data/ms_vars_ts.RData', verbose = verbose)
        load('./data/ms_vars_ws_attr.RData', verbose = verbose)
      } else if(type == 'ts') {
        load('./data/ms_vars_ts.RData', verbose = verbose)
      } else if(type == 'ws') {
        load('./data/ms_vars_ws_attr.RData', verbose = verbose)
      } else {
        stop('type not in accepted options: "ts" for time series variable catalog "ws" for watershed attribute variable catalog, "all" for both')
      }
    },
    error = function(e) {
      stop(paste0('failed to load macrosheds variable catalog into R session'))
    }
    )
  # TODO: add in 'expanded' handling here
  # calculate percentiles of var observations, remove vars in bottom 10% of number of observations
  # this is a proxy for the 'obscurity' of a variable
  if(expanded == FALSE) {
    if(exists('ms_vars_ts') && is.data.frame(get('ms_vars_ts'))) {
      ms_obs_qt = quantile(ms_vars_ts$observations, , probs = c(0.1,0.3,0.5,0.7, 0.9))
      ms_vars_ts <- ms_vars_ts %>%
        filter(observations > ms_obs_qt[[1]])
    } else {
      warning('expanded option only applies to time series variables')
    }
  }

  if(type == 'all') {
    return(list('ms_vars_ts' = ms_vars_ts, 'ms_vars_ws' = ms_vars_ws))
  } else if(type == 'ts') {
    return(ms_vars_ts)
  } else if(type == 'ws') {
    return(ms_vars_ws)
  } else {
    stop('type not in accepted options: "ts" for time series variable catalog "ws" for watershed attribute variable catalog, "all" for both')
  }
}
