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
#' ms_sites <- ms_download_site_data()


ms_download_site_data <- function(){
    
    ms_sites <- readr::read_csv('https://figshare.com/articles/dataset/site_metadata/19358582/files/34961514',
                                col_types = readr::cols())

    return(ms_sites)
}
