#' Download macrosheds variables data table 
#'
#' Download the macrosheds variables table with information on variables codes,
#' units, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of variables, codes,
#'     and units in macrosheds
#' @export
#' @seealso [ms_download_site_data()], [ms_download_core_data()]
#' @examples
#' macrosheds_sites <- ms_downloadvariables()

ms_download_variables <- function(fp = NULL){
    
    ms_vars <- read_csv('https://figshare.com/articles/dataset/site_metadata/19358582/files/34382849',
                        col_types = cols())

    # allow local download if file path supplied
    if(!is.null(fp)) {
      tryCatch(
        expr = {
          write.csv(ms_vars, fp)
        },
        error = function(e) {
          print(paste("file failed to write to:", fp))
        }
      )
    }

    return(ms_vars)
}
