#' load MacroSheds variables data table
#'
#' Load a data.frame of MacroSheds variables with information on variable codes,
#' units, and more.
#'
#' @author Wes Slaughter, \email{weston.slaughter@duke.edu}
#' @author Mike Vlah
#' @author Spencer Rhea
#' @param expanded logical. If TRUE, ms_load_variables will load in the complete variable sheet which includes dozens of
#'     more obscure molecules.
#' @return returns a \code{data.frame} of variables, codes,
#'     and units in MacroSheds format
#' @details Variable metadata are included as an .RData file with the macrosheds package, and
#'    are loaded into the macrosheds namespace on library(macrosheds). This
#'    function simply retrieves a data.frame from the macrosheds namespace and makes it available
#'    in the global environment.
#' @export
#' @seealso [ms_load_sites()], [ms_load_spatial_product()], [ms_load_product()]
#' @examples
#' vars <- ms_load_variables()

ms_load_variables <- function(var_set = 'timeseries'){

    opts <- c('timeseries', 'timeseries_by_site', 'ws_attr')
    if(! var_set %in% opts){
        stop(paste('var_set must be one of', paste(opts, collapse = ', ')))
    }

    requireNamespace('macrosheds', quietly = TRUE)
    
    if(var_set == 'timeseries'){
        
        return(select(macrosheds::ms_vars_ts, -molecule))
        
    } else if(var_set == 'ws_attr'){
        
        return(macrosheds::ms_vars_ws)
        
    } else {
        
        return(macrosheds::ms_var_catalog)
    }
}
