#' Extract MacroSheds variable prefixes
#'
#' Isolate the two-letter prefixes from MacroSheds variable codes. See details.
#'
#' @author Spencer Rhea 
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
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
#'    For more information, see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262).
#' @export
#' @seealso [ms_drop_var_prefix()]
#' @examples
#' macrosheds_vars <- c('GN_NO3_N', 'GN_Mg', 'IS_discharge')
#' macrosheds_prefixs <- extract_var_prefix(x = macrosheds_vars)


ms_extract_var_prefix <- function(x){

    library("dplyr", quietly = TRUE)
    
    if(any(is.na(stringr::str_match(x, '[IGa-z][SNa-z]_.+')))){
        stop('x is not prefixed.')
    }
    
    prefix <- substr(x, 1, 2)
    
    return(prefix)
}