#' Download macrosheds site data table 
#'
#' Download the macrosheds site data table with information on networks, domains,
#'  location of stream gauges, and more
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of all sites in the macrosheds systems 
#'    with their coresponding network, domain, latitude, longitude, area, and 
#'    aditional information 
#' @export
#' @examples
#' macrosheds_sites <- download_ms_site_data()

download_ms_site_data <- function(){
    
    ms_sites <- read.csv('site_data.csv')
    
    # site_temp_loc <- tempfile()
    # download.file(url = 'https://cuahsi.shinyapps.io/macrosheds/_w_4c1747f9/session/85b334ad66b1ff337cfe315dc6315fb5/download/DL_SUBMIT_SITE?w=4c1747f9',
    #               destfile = site_temp_loc,
    #               quiet = TRUE)
    # 
    # ms_sites <- read.csv(site_temp_loc)
    
    return(ms_sites)
}
