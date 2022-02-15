#' Un-scale stream chemical flux by area
#'
#' Macrosheds stream chemical flux is scaled to watershed area (native unit: kg/ha/d).
#' This function converts stream chemistry flux to kg/d.
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param d \code{data.frame} Macrosheds stream chemical flux \code{data.frame}
#'    in the macrosheds native format and units (kg/ha/d).
#' @param site_data character or \code{data.frame}. Optional, either the 
#'    file path to the marocheds site_data file or the \code{data.frame} itself.
#'    If not supplied the file will be downloaded from the web. 
#' @return returns a \code{data.frame} with stream chemical flux in kg/d.
#' @details Macrosheds reports all chemical flux on a per area basis (kg/ha/d) to
#'    facilitate comparison of watersheds of different sizes. Sometimes un-scaled 
#'    fluxes are of interest, this function will convert the units of a native 
#'    macrosheds stream chemical flux table from kg/ha/d to kg/d. To download
#'    macrosheds data, see \code{download_ms_code_data()}.
#' @export
#' @examples
#' d <- load_product(macrosheds_root = 'data/macrosheds_v1', 
#'                   prodname = 'stream_flux_inst_scaled', 
#'                   sort_result = FALSE, 
#'                   domains = 'hbef',
#'                   filter_vars = 'NO3_N')
#'
#' d <- undo_scale_flux_by_area(d)

ms_undo_scale_flux_by_area <- function(d, site_data){
    
    if(missing(site_data)){
        site_data <- ms_download_site_data()
    } else{
            site_data <- try(read_csv(site_data), silent = TRUE)
            site_data_name <- names(site_data)
            if(inherits(site_data, 'try-error') || (!site_data_name %in% c('site_code', 'ws_area_ha'))){
                stop('please enter a correct path to site_data file or omit argument')
            }
    }
    
    sites <- unique(d$site_code)
    
    ws_areas <- site_data %>%
        select(site_code, ws_area_ha) %>%
        filter(site_code %in% !!sites)
    
    d <- d %>%
        left_join(ws_areas,
                  by = 'site_code') %>%
        mutate(val = sw(val * ws_area_ha)) %>%
        select(-ws_area_ha)

    return(d)
}