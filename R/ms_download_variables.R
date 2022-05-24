#' Download macrosheds variables data table 
#'
#' Download a table of MacroSheds variables with information on variable codes,
#' units, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of variables, codes,
#'     and units in MacroSheds format
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples
#' ms_vars <- ms_download_variables()

ms_download_variables <- function(){
    
    ms_vars <- readr::read_csv('https://figshare.com/articles/dataset/variable_metadata/19358585/files/35134504',
                               col_types = readr::cols())

    return(ms_vars)
}
