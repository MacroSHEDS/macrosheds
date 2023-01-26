#' Calculate baseflow
#'
#' Use the Lynne-Hollick baseflow filter, via hydrostats, to perform baseflow separation 
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param discharge \code{data.frame}. A MacroSheds discharge table of one site
#' @param alpha Numeric. Default 0.975, alpha value for the Lynne-Hollick baseflow filter. See 
#'    details for more information.
#' @param n.reflected Numeric. Default 30, see hydrostats::baseflow documentation for more information.
#' @return returns a discharge \code{data.frame} in MacroSheds format with the added column of "baseflow"
#' @details This function uses the Lynne-Hollick baseflow filter to separate baseflow 
#'    and stormflow. The underlying package hydrostats is used for this calculation. Additional 
#'    information on alpha values and n.reflected can be found using ?hydrostats::baseflows
#' @export
#' @examples
#' q = macrosheds::ms_load_product(
#'     macrosheds_root = 'my/ms/root/',
#'     prodname = 'discharge',
#'     domains = 'hbef')
#' ms_separate_baseflow(q)

ms_separate_baseflow <- function(discharge, alpha = 0.975, n.reflected = 30) {

    requireNamespace("dplyr", quietly = TRUE)
    
    # Checks
    if(!all(c('site_code', 'datetime', 'val', 'var') %in% names(discharge))){
        stop('discharge files must be in MacroSheds format with names: site_code, datetime, val, var')
    }
    
    if(!length(unique(discharge$site_code) == 1)){
        stop('only one site can be run at a time')
    }

    hydro_input <- tibble(Date = lubridate::as_date(discharge$datetime),
                          Q = discharge$val)
    
    hydro_output <- hydrostats::baseflows(flow.ts = hydro_input,
                                          a = alpha,
                                          n.reflected = n.reflected,
                                          ts = 'daily') %>%
        select(datetime = Date, baseflow = bf)
    
    bf_final <- discharge %>%
        left_join(., hydro_output, by = 'datetime')
    
    return(bf_final)
}

