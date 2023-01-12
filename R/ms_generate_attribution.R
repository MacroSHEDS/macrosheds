#' Easily cite and/or acknowledge primary sources of the MacroSheds dataset
#'
#' Also returns contact information, URLs, DOIs, and intellectual rights
#' details, all based on an input data.frame in MacroSheds format.
#'
#' @author Mike Vlah, vlahm13@@gmail.com
#' @author Wes Slaughter
#' @param chemistry \code{data.frame}. A \code{data.frame} of precipitation or
#' @return returns a 
#'    where 
#' @details
#' Chemical 
#' [ms_undo_scale_flux_by_area()].
#'
#' Before running [ms_calc_flux()], ensure both \code{q} and
#' @seealso [ms_synchronize_timestep()], 
#' @export
#' @examples
#' #' ### Load some MacroSheds data:
#' ms_root = 'data/macrosheds'

#library(macrosheds)

#d2 <- macrosheds::ms_load_product(macrosheds_root = '../../../r_package/data/ms_test',
#                                            prodname = 'precip_chemistry',
#                                            domains = c('hbef'),
#                                            warn = F)

# conf <- jsonlite::fromJSON('../../config.json',
#                            simplifyDataFrame = FALSE)
# attrib_data <- googlesheets4::read_sheet(
#     conf$site_doi_license_gsheet,
#     skip = 5,
#     na = c('', 'NA'),
#     col_types = 'c'
# )

ms_generate_attribution <- function(d){
    
    sitevars <- d %>% 
        mutate(var = macrosheds::ms_drop_var_prefix(var)) %>%
        distinct(site_code, var)
    
    if(! is.null(site_data)){
        if(! all(c('domain', 'site_code') %in% colnames(site_data))){
            stop('site_data invalid; see ms_download_site_data, or leave this parameter unassigned.')
        }
    } else {
        message('Retrieving site data')
        site_data <- macrosheds::ms_download_site_data()
    }
    
    # if(! is.null(attrib_data)){
    #     if(! all(c('domain', 'macrosheds_prodcode') %in% colnames(attrib_data))){
    #         stop('attrib_data invalid; see ms_download_attribution, or leave this parameter unassigned.')
    #     }
    # } else {
    #     message('Retrieving attribution data')
    #     attrib_data <- macrosheds::ms_download_attribution()
    # }
    # 
    # sitevars <- left_join(sitevars, select(site_data, domain, site_code), by = 'site_code')
    # 
    # #read prodname prodcode mapping
    # 
    # sitevars <- left_join(sitevars, attrib_data, by = c('domain', 'prod'))
    
    #filter
    #acknowledge can't distinguish precip from stream
    
}
    
