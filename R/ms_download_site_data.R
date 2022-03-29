#' Download MacroSheds site metadata table 
#'
#' Download the MacroSheds site data table, including information on networks, domains,
#' stream gauge locations, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{tibble} of metadata for all MacroSheds sites,
#'    including network, domain, location, area, and additional information.
#'    For definitions, see PLACEHOLDER0.
#' @export
#' @seealso [ms_download_variables()], [ms_download_core_data()]
#' @examples
#' macrosheds_sites <- ms_downloadsite_data()

ms_download_site_data <- function(){
    
    ms_sites <- read_csv('https://figshare.com/articles/dataset/site_metadata/19358582/files/34382846',
                         col_types = cols())
    
    return(ms_sites)
}
