#' Drop MacroSheds variable prefixes
#'
#' Remove the two-letter prefixes from MacroSheds variable codes. See details.
#'
#' @author Spencer Rhea 
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param x character MacroSheds variable code with a prefix.
#' @return Returns a vector of MacroSheds variable codes without their two character prefixes.
#'    If prefixes are missing, returns \code{x} with an warning.
#' @details MacroSheds variable codes all include a two-letter prefix to denote the
#'    sampling regimen. The first letter is either I for "installed"
#'    or G for "grab," denoting if this variable was recorded by a continuously
#'    monitoring piece of equipment, or manually by a human. 
#'    The second position is either S for "sensor" or N for "non-sensor".
#'    Non-sensor samples are generally measured analytically, in a laboratory.
#'    For more information, see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262).
#' @export
#' @seealso [ms_extract_var_prefix()]
#' @examples
#' macrosheds_vars <- c('GN_NO3_N', 'GN_Mg', 'IS_discharge')
#' macrosheds_vars <- drop_var_prefix(x = macrosheds_vars)

ms_drop_var_prefix <- function(x){

    library("dplyr", quietly = TRUE)
    
    if(any(is.na(stringr::str_match(x, '[IGa-z][SNa-z]_.+')))){
        warning('x is not prefixed.')
        return(x)
    }
    
    unprefixed <- substr(x, 4, nchar(x))
    
    return(unprefixed)
}