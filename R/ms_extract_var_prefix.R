#' Extract MacroSheds variable prefixes
#'
#' Isolate the two-letter prefixes from MacroSheds variable codes. See details.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param x character. MacroSheds variable code with a prefix.
#' @return Returns a vector of two-character prefixes. If x is not
#'    prefixed, returns an error.
#' @details MacroSheds variable codes all include a two-letter prefix to denote the
#'    sampling regimen. The first letter is either I for "installed"
#'    or G for "grab," denoting if this variable was recorded by a continuously
#'    monitoring piece of equipment, or manually by a human. 
#'    The second position is either S for "sensor" or N for "non-sensor".
#'    Non-sensor samples are generally measured analytically, in a laboratory.
#'    For more information, see [MacroSheds documentation](https://doi.org/10.6084/m9.figshare.c.5621740).
#' @export
#' @seealso [ms_drop_var_prefix()]
#' @examples
#' macrosheds_vars <- c('GN_NO3_N', 'GN_Mg', 'IS_discharge')
#' macrosheds_prefixs <- extract_var_prefix(x = macrosheds_vars)


ms_extract_var_prefix <- function(x){

    requireNamespace("dplyr", quietly = TRUE)
    
    if(any(is.na(stringr::str_match(x, '[IGa-z][SNa-z]_.+')))){
        stop('x is not prefixed.')
    }
    
    prefix <- substr(x, 1, 2)
    
    return(prefix)
}