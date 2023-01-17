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

# d <- macrosheds::ms_load_product(macrosheds_root = '../r_package/data/ms_test',
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

ms_generate_attribution <- function(d, chem_source = 'both',
                                    # return_format = 'basic_dataframe',
                                    include_ws_attr = TRUE){
    
    if(! chem_source %in% c('stream', 'precip', 'both')){
        stop('chem_source must be one of "stream", "precip", or "both"')
    }
    
    # rtn_fmts <- c('basic_dataframe', 'complete_dataframe', 'attribution_only')
    # if(! return_format %in% rtn_fmts){
    #     stop(paste0('return_format must be one of "',
    #                paste(rtn_fmts, collapse = '", "'), '"'))
    # }
    
    attrib <- list()
    
    if(missing(d)){
        
        message('d (data.frame in MacroSheds format) not supplied. Returning all records')
        
        attrib$IR_timeseries <- attrib_ts_data
        
        if(include_ws_attr){
            
            attrib$IR_ws_attr <- attrib_ws_data
            attrib$acknowledgements <- format_acknowledgements(attrib_ts_data,
                                                               ws_attr = TRUE)
            attrib$bibliography <- format_bibliography(attrib_ts_data,
                                                       ws_attr = TRUE)
        } else {
            
            attrib$acknowledgements <- format_acknowledgements(attrib_ts_data)
            attrib$bibliography <- format_bibliography(attrib_ts_data)
        }
        
        return(attrib)
    }
    
    sitevars <- d %>% 
        mutate(var = macrosheds::ms_drop_var_prefix(var)) %>%
        mutate(var = case_when(var == 'precipitation' ~ 'precipitation',
                               var == 'discharge' ~ 'discharge',
                               TRUE ~ 'chemistry')) %>% 
        distinct(site_code, var)
    
    if('chemistry' %in% sitevars$var){
        
        if(chem_source == 'precip'){
            sitevars$var[sitevars$var == 'chemistry'] <- 'precip_chemistry'
        } else if(chem_source == 'stream'){
            sitevars$var[sitevars$var == 'chemistry'] <- 'stream_chemistry'
        } else {
            sv0 <- sitevars
            sv0$var[sv0$var == 'chemistry'] <- 'stream_chemistry'
            sitevars$var[sitevars$var == 'chemistry'] <- 'precip_chemistry'
            sitevars <- bind_rows(sv0, sitevars)
        }
    }
    
    sitevars <- left_join(sitevars,
                          select(ms_site_data, domain, site_code),
                          by = 'site_code') %>% 
        filter(! is.na(domain)) %>% 
        distinct(domain, var)
    
    dmns <- unique(sitevars$domain)
    sitevars <- tibble(domain = rep(dmns, each = 3),
           var = rep(c('ws_boundary', 'stream_gauge_locations', 'precip_gauge_locations'),
                     times = length(dmns))) %>% 
        bind_rows(sitevars)

    sitevars <- left_join(sitevars, attrib_ts_data,
                          by = c('domain', var = 'macrosheds_prodname')) %>% 
        filter(! is.na(network))
        
        
    attrib$IR_timeseries <- sitevars
    
    if(include_ws_attr){
        attrib$IR_ws_attr <- attrib_ws_data
        attrib$acknowledgements <- format_acknowledgements(sitevars,
                                                           ws_attr = TRUE)
        attrib$bibliography <- format_bibliography(sitevars,
                                                   ws_attr = TRUE)
    } else {
        
        attrib$acknowledgements <- format_acknowledgements(attrib_ts_data)
        attrib$bibliography <- format_bibliography(attrib_ts_data)
    }
    
    return(attrib)
}
    
