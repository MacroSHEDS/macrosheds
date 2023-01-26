#' Easily cite and/or acknowledge primary sources of the MacroSheds dataset
#'
#' Also returns contact information, URLs, DOIs, and intellectual rights
#' details, all based on an input data.frame in MacroSheds format.
#'
#' @author Mike Vlah, vlahm13@@gmail.com
#' @author Wes Slaughter
#' @param d \code{data.frame}. A \code{data.frame} in MacroSheds format (i.e. with
#'   at least columns datetime, site_code, var.). If omitted, all attribution
#'   records will be returned.
#' @param chem_source \code{character}. Whether \code{d} includes "stream" or "precip"
#'   chemistry data (as these cannot be distinguished by \code{ms_generate_attribution}).
#'   If both are included, use "both". If neither, this parameter will be ignored. If
#'   \code{d} is not provided, this parameter pertains to the full MacroSheds dataset.
#' @param include_ws_attr \code{logical}. If FALSE, attribution information will
#'   be generated only for MacroSheds time-series data products. If TRUE,
#'   attribution information will be generated for watershed attribute (ws_attr)
#'   products as well. If you are using our ws_attr products in any capacity,
#'   set this to TRUE.
#' @param abide_by \code{character}. Primary source Intellectual Rights (IR)
#'   stipulations may use language like "should" or "encouraged to", or they
#'   might use "must", "required to", etc. If you set this parameter to
#'   "suggestions", all IR clauses will be returned. If you set it to
#'   "requirements only", clauses with mild language will be filtered out.
#'   See details.
#' @param write_to_dir \code{character}. A path to an existing directory where
#'   attribution files will be written.
#'   A new directory called \code{macrosheds_attribution_information} will be created there.
#'   If NULL (the default), all attribution
#'   information will be returned as a list. If specified, some information
#'   will still be returned as a list, including primary source contact information
#'   and DOIs. See Value.
#' @details
#'   Note that the world of data IR is still being constructed, and there's a lot
#'   of legal gray area around whether end-users of data syntheses like
#'   MacroSheds are held to the same expectations as we were when we assembled
#'   MacroSheds. We recommend acknowledging/citing our primary sources in any case. Whether
#'   you adhere to expectations about e.g. contacting primary sources to ask
#'   permission to use their data... well, we leave that up to you, because the
#'   fact is you're using products derived from their data. If you're using
#'   much or all of the MacroSheds dataset for an analysis, it's not reasonable
#'   to ask you to contact 20 different institutions and ask for various permissions.
#'   However, if you're only using one or a few MacroSheds domains in your analysis,
#'   it seems only right that you fulfill all of their IR clauses, just as if you
#'   were interacting directly with primary source data. Still, pay special attention
#'   to the noncommercial and sharealike licenses attached to some of the MacroSheds domains.
#'   These licenses are legally black-and-white, and you can definitely
#'   get in trouble if you disregard them.
#' @return Returns a list. If \code{write_to_dir} is not provided, this list contains
#'   the full output:
#' * acknowledgements: a string of acknowledgement text
#' * bibliography: a vector of BibTeX entries
#' * intellectual_rights_explanations: a vector of definitions pertaining to 
#'   \code{intellectual_rights_notifications}
#' * intellectual_rights_notifications: a list of tibbles containing special
#'   notifications
#' * full_details_timeseries: a tibble containing full IR, URL, and contact information
#'   for each primary source time-series dataset
#' * full_details_ws_attr: a tibble containing full IR, URL, and contact information
#'   for each primary source watershed attribute dataset
#'       
#'  If \code{write_to_dir} is provided, this list contains only \code{full_details_timeseries}
#'  and \code{full_details_ws_attr}, and all other information is written to files
#'  in \code{write_to_dir/macrosheds_attribution_information}.
#' @seealso [ms_download_core_data()]
#' @export
#' @examples
#' d <- macrosheds::ms_load_product(
#'     macrosheds_root = 'my/macrosheds/root/',
#'     prodname = 'precip_chemistry',
#'     domains = 'hbef')
#' 
#' ms_generate_attribution(d, chem_source = 'precip',
#'                         write_to_dir = '~/projects/hbef_precip/')

ms_generate_attribution <- function(d, chem_source = 'both',
                                    include_ws_attr = TRUE,
                                    abide_by = 'requirements only',
                                    write_to_dir = NULL){

    requireNamespace("dplyr", quietly = TRUE)
    
    if(! missing(d) && (! inherits(d, 'data.frame') | ! all(c('site_code', 'datetime', 'var') %in% colnames(d)))){
        stop('d must be a data.frame in MacroSheds format (with at least datetime, site_code, and var columns)')
    }
    
    if(! chem_source %in% c('stream', 'precip', 'both')){
        stop('chem_source must be one of "stream", "precip", or "both"')
    }
    
    if(! abide_by %in% c('suggestions', 'requirements only')){
        stop('abide_by must be either "suggestions" or "requirements only"')
    }
    
    if(! is.null(write_to_dir) && ! inherits(write_to_dir, 'character')){
        stop('write_to_dir must be NULL or a valid path.')
    }
    
    if(! is.logical(include_ws_attr)){
        stop('include_ws_attr must be TRUE or FALSE.')
    }
    
    if(! is.null(write_to_dir)){
        if(! dir.exists(write_to_dir)){
            stop(paste(write_to_dir, 'does not exist. Make sure write_to_dir is a valid directory, not a file.'))
        }
    }

    requireNamespace('macrosheds', quietly = TRUE)
    
    attrib <- list()
    
    if(missing(d)){
        
        message('d (data.frame in MacroSheds format) not supplied. Returning all records')
        
        attrib$acknowledgements <- macrosheds:::format_acknowledgements(
            macrosheds::attrib_ts_data,
            ws_attr = include_ws_attr)
        
        attrib$bibliography <- macrosheds:::format_bibliography(
            macrosheds::attrib_ts_data,
            ws_attr = include_ws_attr)
        
        ir <- macrosheds:::format_IR(
            macrosheds::attrib_ts_data,
            ws_attr = include_ws_attr,
            abide_by = abide_by)
        
        attrib$intellectual_rights_notifications <- ir$intellectual_rights
        attrib$intellectual_rights_explanations <- ir$IR_explanations
        
        attrib$full_details_timeseries <- macrosheds::attrib_ts_data
        
        if(include_ws_attr){
            attrib$full_details_ws_attr <- macrosheds::attrib_ws_data
        }
        
        if(is.null(write_to_dir)){
            return(attrib)
        } else {
            macrosheds:::attrib_output_write(attrib, write_to_dir)
            message(paste0('Output files written to ', write_to_dir,
                           '/macrosheds_attribution_information/'))
        }
        
        return(attrib[c('full_details_timeseries', 'full_details_ws_attr')])
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
                          select(macrosheds::ms_site_data, domain, site_code),
                          by = 'site_code') %>% 
        filter(! is.na(domain)) %>% 
        distinct(domain, var)
    
    dmns <- unique(sitevars$domain)
    sitevars <- tibble(domain = rep(dmns, each = 3),
           var = rep(c('ws_boundary', 'stream_gauge_locations', 'precip_gauge_locations'),
                     times = length(dmns))) %>% 
        bind_rows(sitevars)

    sitevars <- left_join(sitevars, macrosheds::attrib_ts_data,
                          by = c('domain', var = 'macrosheds_prodname')) %>% 
        rename(macrosheds_prodname = var) %>%
        relocate(macrosheds_prodname, .after = macrosheds_prodcode) %>% 
        filter(! is.na(network))

    attrib$acknowledgements <- macrosheds:::format_acknowledgements(
        sitevars,
        ws_attr = include_ws_attr)
    
    attrib$bibliography <- macrosheds:::format_bibliography(
        sitevars,
        ws_attr = include_ws_attr)
    
    ir <- macrosheds:::format_IR(
        sitevars,
        ws_attr = include_ws_attr,
        abide_by = abide_by)
    
    attrib$intellectual_rights_notifications <- ir$intellectual_rights
    attrib$intellectual_rights_explanations <- ir$IR_explanations
    
    attrib$full_details_timeseries <- sitevars
    
    if(include_ws_attr){
        attrib$full_details_ws_attr <- macrosheds::attrib_ws_data
    }
    
    if(is.null(write_to_dir)){
        return(attrib)
    } else {
        macrosheds:::attrib_output_write(attrib, write_to_dir)
        message(paste0('Output files written to ', write_to_dir,
                       '/macrosheds_attribution_information/'))
    }
    
    return(attrib[c('full_details_timeseries', 'full_details_ws_attr')])
}
    
