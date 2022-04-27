#' Download macrosheds site data table 
#'
#' Download the macrosheds site data table with information on networks, domains,
#'  location of stream gauges, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of all sites in the macrosheds systems 
#'    with their corresponding network, domain, latitude, longitude, area, and 
#'    additional information.
#' @export
#' @examples
#' macrosheds_sites <- download_ms_site_data()


ms_download_site_data <- function(){
    
    ms_sites <- read_csv('https://figshare.com/articles/dataset/site_metadata/19358582/files/34382846',
                         col_types = cols())
    
    return(ms_sites)
}