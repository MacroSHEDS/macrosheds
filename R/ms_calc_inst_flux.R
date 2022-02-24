#' Calculates chemical fluxes
#' 
#' Calculates stream discharge and precipitation fluxes of chemicals from Q (discharge 
#' or precipitation) and chemistry data 
#'
#' @author Spencer Rhea, spencerrhea41@@gmail.com
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param chemistry \code{data.frame}. A \code{data.frame} of precipitation or 
#'    stream chemistry data in macrosheds format and in units of mg/l.
#' @param q \code{data.frame}. A \code{data.frame} of precipitation or stream
#'    discharge in macrosheds format and in units of mm or L/s respectively.
#' @param data_type character. Either precipitation or discharge. 
#' @param verbose logical. Default true, should function print information to 
#'    console. 
#' @return returns a \code{tibble} of stream instantaneous flux for every timestep 
#'    where discharge/precipitation and chemistry are reported in units of kg/ha/timestep. 
#' @details Chemical flux is calculated by multiplying chemical concentration by flow
#'    of water (flux = concentration * flow). The output units depend on the time 
#'    interval the input data is collected at. If data is collected at 15 minutes 
#'    the out put will be in kg/ha/15 minutes and if collected daily units will 
#'    be in kg/day. 
#'    
#'    Before imputing data into \code{ms_calc_inst_flux()}, insure all both q and
#'    chemistry data is in the same time step. See \code{ms_synchronize_timestep} to 
#'    take data of different intervals and interpolate or summarize data to different 
#'    sampling intervals. Also insure chemistry units are in mg/l, see \code{ms_conversions}
#'    for unit conversions. 
#' @export
#' @examples
#' 
#' ms_download_core_data(macrosheds_root = 'data/ms_test',
#'                       domains = 'hbef')
#' chemistry <- macrosheds::ms_load_product('data/ms_test/',
#'                                          'stream_chemistry',
#'                                          site_codes = c('w1', 'w3', 'w6'),
#'                                          filter_vars = c('NO3_N', 'Cl', 'Na'),
#'                                          warn = F)
#' 
#' q <- macrosheds::ms_load_product('data/ms_test/',
#'                                  'discharge',
#'                                  site_codes = c('w1', 'w3', 'w6'),
#'                                  warn = F)
#' 
#' flux <- ms_calc_inst_flux(chemistry = chemistry, q = q, q_type = 'discharge')

ms_calc_inst_flux <- function(chemistry, q, q_type, verbose = TRUE) {
    
    #### Checks
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(chemistry))){
        stop('the chemistry file must be either a preicpitation or stream chamistry dataset in macrosheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum) ')
    }
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(q))){
        stop('the chemistry file must be either a preicpitation or stream discharge dataset in macrosheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum) ')
    }
    if(! grepl('(precipitation|discharge)', q_type)){
        stop('q_type must be discharge or precipitation')
    }
    if(! 'POSIXct' %in% class(q$datetime)){
        q$datetime <- as.POSIXct(q$datetime)
    }
    if(! 'POSIXct' %in% class(chemistry$datetime)){
        chemistry$datetime <- as.POSIXct(chemistry$datetime)
    }
    
    # Check the intervals are the same in both chemistry and q
    q_interval <- Mode(diff(as.numeric(q$datetime)))
    
    interval <- case_when(q_interval == 86400 ~ 'daily',
                          q_interval == 3600 ~ 'hourly',
                          q_interval == 1800 ~ '30 minute',
                          q_interval == 960 ~ '15 minute',
                          q_interval == 600 ~ '10 minute',
                          q_interval == 300 ~ '5 minute',
                          q_interval == 60 ~ '1 minute')
    
    # q_interval <- errors::as.errors(q_interval)
    
    
    flow_is_highres <- Mode(diff(as.numeric(q$datetime))) <= 15 * 60
    if(is.na(flow_is_highres)) { flow_is_highres <- FALSE }
    
    if(is.na(interval)) {
        stop(paste0('data is not in a standard time step, data must be in the timesteps',
                    ' of: daily, hourly, 30 minute, 15 minute, 10, minute, 5 minute, or 1 minute.',
                    ' See macrosheds::ms_synchronize_timestep() to standardize your data.'))
    } else if(verbose) {
        print(paste0('q data is reported at a ', interval, ' timestep'))
    }
    
    # add errors if they don't exist 
    if('val_err' %in% names(chemistry)){
        errors::errors(chemistry$val) <- chemistry$val_err
        
        chemistry <- chemistry %>%
            select(-val_err)
        
    } else if(all(errors::errors(chemistry$val) == 0)){
        errors::errors(chemistry$val) <- 0
    }
    
    if('val_err' %in% names(q)){
        errors::errors(q$val) <- q$val_err
        
        q <- q %>%
            select(-val_err)
        
    } else if(all(errors::errors(q$val) == 0)){
        errors::errors(q$val) <- 0
    }
    
    # calc flux
    sites <- unique(chemistry$site_code)
    
    all_sites_flux <- tibble()
    for(s in 1:length(sites)) {
        
        site <- sites[s]
        
        site_chem <- chemistry %>%
            filter(site_code == !!site)
        
        site_q <- q %>%
            filter(site_code == !!site)
        
        daterange <- range(site_chem$datetime)
        
        site_q <- site_q %>%
            filter(
                site_code == !!site,
                datetime >= !!daterange[1],
                datetime <= !!daterange[2])
        
        if(nrow(site_q) == 0) { return(NULL) }
        
        chem_split <- site_chem %>%
            group_by(var) %>%
            arrange(datetime) %>%
            dplyr::group_split() %>%
            as.list()
        
        # Loop though all variables
        for(i in 1:length(chem_split)) {
            
            chem_chunk <- chem_split[[i]]
            
            chem_is_highres <- Mode(diff(as.numeric(chem_chunk$datetime))) <= 15 * 60
            if(is.na(chem_is_highres)) { chem_is_highres <- FALSE}
            
            #if both chem and flow data are low resolution (grab samples),
            #   let approxjoin_datetime match up samples with a 12-hour gap. otherwise the
            #   gap should be 7.5 mins so that there isn't enormous duplication of
            #   timestamps where multiple high-res values can be snapped to the
            #   same low-res value
            if(! chem_is_highres && ! flow_is_highres) {
                join_distance <- c('12:00:00')#, '%H:%M:%S')
            } else {
                join_distance <- c('7:30')#, '%M:%S')
            }
            
            chem_split[[i]] <- approxjoin_datetime(x = chem_chunk,
                                                   y = site_q,
                                                   rollmax = join_distance,
                                                   keep_datetimes_from = 'x')
            
            if(q_type == 'discharge'){
                chem_split[[i]] <- chem_split[[i]] %>%
                    mutate(site_code = site_code_x,
                           var = var_x,
                           # kg/interval = mg/L *  L/s  * q_interval / 1e6
                           val = val_x * val_y * errors::as.errors(q_interval) / errors::as.errors(1e6),
                           ms_status = numeric_any_v(ms_status_x, ms_status_y),
                           ms_interp = numeric_any_v(ms_interp_x, ms_interp_y)) %>%
                    select(-starts_with(c('site_code_', 'var_', 'val_',
                                          'ms_status_', 'ms_interp_'))) %>%
                    filter(! is.na(val)) %>% #should be redundant
                    arrange(datetime)
            } else {
                chem_split[[i]] <- chem_split[[i]] %>%
                    mutate(site_code = site_code_x,
                           var = var_x,
                           # kg/interval/ha = mg/L *  mm/x * ha/100
                           val = val_x * val_y / errors::as.errors(100),
                           ms_status = numeric_any_v(ms_status_x, ms_status_y),
                           ms_interp = numeric_any_v(ms_interp_x, ms_interp_y)) %>%
                    select(-starts_with(c('site_code_', 'var_', 'val_',
                                          'ms_status_', 'ms_interp_'))) %>%
                    filter(! is.na(val)) %>% #should be redundant
                    arrange(datetime)
            }
        }
        
        flux <- chem_split %>%
            purrr::reduce(bind_rows) %>%
            arrange(site_code, var, datetime)
        
        all_sites_flux <- rbind(all_sites_flux, flux)
        
    }
    
    if(nrow(all_sites_flux) == 0) { return(NULL) }
    
    all_sites_flux$val_err <- errors::errors(all_sites_flux$val)
    all_sites_flux$val <- errors::drop_errors(all_sites_flux$val)
    
    return(all_sites_flux)
}
