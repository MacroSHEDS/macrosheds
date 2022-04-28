#' Download MacroSheds site data table
#'
#' Download the MacroSheds site data table with information on networks, domains,
#'  location of stream gauges, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of all sites in the MacroSheds systems
#'    with their corresponding network, domain, latitude, longitude, area, and 
#'    additional information.
#' @export
#' @examples
#' macrosheds_sites <- ms_download_site_data()


ms_download_site_data <- function(fp = NULL){
    
    ms_sites <- read_csv('https://figshare.com/articles/dataset/site_metadata/19358582/files/34382846',
                         col_types = cols())

    # allow local download if file path supplied
    if(!is.null(fp)) {
      tryCatch(
        expr = {
          write.csv(ms_sites, fp)
        },
        error = function(e) {
          print(paste("file failed to write to:", fp))
        }
      )
    }

    return(ms_sites)
}
