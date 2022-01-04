#' Download macrosheds varibles data table 
#'
#' Download the macrosheds variables table with information on variables codes,
#' units, and more 
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of variables, codes,
#'     and units in macrosheds
#' @export
#' @examples
#' macrosheds_sites <- download_ms_variables()

ms_download_variables <- function(){
    
    ms_sites <- read.csv('variables.csv')
    
    # site_temp_loc <- tempfile()
    # download.file(url = 'https://cuahsi.shinyapps.io/macrosheds/_w_4c1747f9/session/85b334ad66b1ff337cfe315dc6315fb5/download/DL_SUBMIT_SITE?w=4c1747f9',
    #               destfile = site_temp_loc,
    #               quiet = TRUE)
    # 
    # ms_sites <- read.csv(site_temp_loc)
    
    return(ms_sites)
}
