#' Download macrosheds site metadata table 
#'
#' Download the macrosheds site data table, including information on networks, domains,
#' stream gauge locations, and more.
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{tibble} of metadata for all macrosheds sites,
#'    including network, domain, location, area, and additional information.
#'    For definitions, see PLACEHOLDER0.
#' @export
#' @seealso [ms_download_variables()], [ms_download_core_data()]
#' @examples
#' macrosheds_sites <- ms_downloadsite_data()

ms_download_site_data <- function(){
    
    ms_sites <- read_csv('site_data.csv', col_types = cols())
    
    # site_temp_loc <- tempfile()
    # download.file(url = 'https://cuahsi.shinyapps.io/macrosheds/_w_4c1747f9/session/85b334ad66b1ff337cfe315dc6315fb5/download/DL_SUBMIT_SITE?w=4c1747f9',
    #               destfile = site_temp_loc,
    #               quiet = TRUE)
    # 
    # ms_sites <- reads_.csv(site_temp_loc)
    
    return(ms_sites)
}
