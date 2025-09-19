#' Extract MacroSheds variable prefixes (DEPRECATED)
#'
#' Isolate the two-letter prefixes from MacroSheds variable codes. Retained for legacy use, but will be removed in a later version. See details.
#'
#' @author Spencer Rhea
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param x character. MacroSheds variable code with a prefix.
#' @return Returns a vector of two-character prefixes. If x is not
#'    prefixed, returns an error.
#' @details In version 1 of the MacroSheds dataset, variable codes all include a two-letter prefix to denote the
#'    sampling regimen. The first letter is either I for "installed"
#'    or G for "grab," denoting if this variable was recorded by a continuously
#'    monitoring piece of equipment, or manually by a human.
#'    The second position is either S for "sensor" or N for "non-sensor".
#'    Non-sensor samples are generally measured analytically, in a laboratory.
#'    In version 2 and later, this system has been simplified and replaced by a single,
#'    logical column called "grab_sample".
#' @export
#' @seealso [ms_drop_var_prefix()]
#' @examples
#' macrosheds_vars <- c('GN_NO3_N', 'GN_Mg', 'IS_discharge')
#' macrosheds_prefixs <- extract_var_prefix(x = macrosheds_vars)


ms_extract_var_prefix <- function(x){

    library("dplyr", quietly = TRUE)

    warning('Since version 2 of the MacroSheds dataset, variable prefixes have been replaced by a "grab_sample" column. ms_extract_var_prefix is deprecated and will be removed in a later version of this package. Please use ms_download_core_data to upgrade to dataset v2.')

    if(any(is.na(stringr::str_match(x, '^[IGa-z][SNa-z]_.+')))){
        stop('x is not prefixed.')
    }

    prefix <- substr(x, 1, 2)

    return(prefix)
}
