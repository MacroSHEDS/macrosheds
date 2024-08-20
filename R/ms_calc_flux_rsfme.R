#' Calculate daily, monthly or annual solute fluxes (loads)
#'
#' Determines solute fluxes from daily Q (stream discharge
#' or precipitation depth) and corresponding chemistry data using
#' six available methods.
#'
#' @author Wes Slaughter, \email{wslaughter@@berkeley.edu}
#' @author Nick Gubbins, \email{gubbinsnick@@gmail.com}
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Spencer Rhea 
#'
#' @param chemistry A \code{data.frame} or \code{tibble} of precipitation or
#'    stream chemistry data in MacroSheds format (see details) and in units of mg/L.
#' @param q A \code{data.frame} or \code{tibble} of stream
#'    discharge (L/s) or precipitation depth (mm) in MacroSheds format (see detaails).
#' @param method character either 'simple' for for daily flux, or a vector including any
#'    combination of the following for computing monthly or annual cumulative flux, AKA load: 'pw', 'rating', 'composite', 'beale', 'average'. See details.
#' @param aggregation character. For method='simple', this argument should remain NULL. If using any
#'    other method(s), specify either "monthly" or "annual". 
#' @param good_year_check logical. definition forthcoming
#' @param verbose logical. control the level of informational printing.
#' @return a \code{tibble} of stream or precipitation solute flux (if method="simple") or monthly or annual load. Output units are kg/ha/timestep.
#' @details
#'
#' The \code{chemistry} and \code{q} parameters require inputs in MacroSheds format, which is the format returned by ms_load_product for core time-series data. MacroSheds format is:
#' | header value  | column_definition |
#' | ------------- | ----------------- |
#' | datetime      | Date and time in UTC. Time is specified in order to accommodate subdaily time-series data in future updates, though at present all time components are 00:00:00. |
#' | site_code     | A unique identifier for each MacroSheds site. Identical to primary source site code where possible. See sites.csv metadata on EDI or run ms_load_sites() from the macrosheds package for more information. |
#' | var           | Variable code, including sample type prefix (described in "Tracking of Sampling Methods for Each Record" section). see variables_timeseries.csv on EDI or run ms_load_variables() from the macrosheds package for more information. |
#' | val           | The data value. |
#' | ms_status     | QC flag. See "Technical Validation" section. Lowercase "ms" here stands for "MacroSheds." |
#' | ms_interp     | Imputation flag, described in "Temporal Imputation and Aggregation" section. |
#' | val_err       | The combined standard uncertainty associated with the corresponding data point, if estimable. See “Detection Limits and Propagation of Uncertainty” section for details. |
#'
#' `method` = 'simple' computes flux by multiplying solute concentration by discharge at each timestep.
#' The output units depend on the time
#' interval at which input data are collected. The resulting flux units will always be
#' kg/ha/T, where T is the time interval of the input (as of MacroSheds version 1, all time-series data are provided at a daily interval).
#' For stream_chemistry and discharge, flux is calculated as: 
#' kg/ha/T = mg/L * L/s * T / 1e6 / ws_area.
#' For precipitation chemistry and precip depth, flux is calculated as: 
#' kg/ha/T (kg/ha/T = mg/L * mm/T / 100).
#' You can convert between kg/ha/T and kg/T using [ms_scale_flux_by_area()] and
#' [ms_undo_scale_flux_by_area()].
#'
#' Annual/monthly cumulative flux, or load, can also be estimated with this function, using one of five `method`s detailed below. Consult Figure 10 in Aulenbach et al. 2016 for guidance on method selection. See Figure 1 for some intuition.
#'  + 'pw': period-weighted method, described in Aulenbach et al. 2016. Here, C is linearly interpolated. This option uses package RiverLoad.
#'  + 'rating': AKA regression-model method, described in Aulenbach et al. 2016. This option uses package RiverLoad.
#'  + 'composite': composite method, described in Aulenbach et al. 2016. This option uses package RiverLoad.
#'  + 'beale': Beale ratio estimator, described in Meals et al. 2013. This option uses package RiverLoad.
#'  + 'average': mean Q over the aggregation period times mean C over the aggregation period
#'
#' All output units are kg/ha/T, where T is the aggregation period.
#'
#' References:
#'
#'  + Aulenbach, B. T., Burns, D. A., Shanley, J. B., Yanai, R. D., Bae, K., Wild, A. D., ... & Yi, D. (2016). Approaches to stream solute load estimation for solutes with varying dynamics from five diverse small watersheds. Ecosphere, 7(6), e01298.
#'  +Meals, D. W., Richards, R. P., & Dressing, S. A. (2013). Pollutant load estimation for water quality monitoring projects. Tech Notes, 8, 1-21.
#'
#' Before running [ms_calc_flux_rsfme()], ensure both \code{q} and
#' \code{chemistry} have the same time interval. See [ms_synchronize_timestep()].
#' Also ensure chemistry units are mg/L. See [ms_conversions()].
#' @seealso [ms_synchronize_timestep()], [ms_conversions()], [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @examples
#' #' ### Load some MacroSheds data:
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = 'hbef')
#' chemistry <- ms_load_product(macrosheds_root = ms_root,
#'                              prodname = 'stream_chemistry',
#'                              site_codes = c('w1', 'w3', 'w6'),
#'                              filter_vars = c('NO3_N', 'Cl', 'Na'))
#'
#' q <- ms_load_product(macrosheds_root = ms_root,
#'                      prodname = 'discharge',
#'                      site_codes = c('w1', 'w3', 'w6'))
#'
#' flux <- ms_calc_flux_rsfme(chemistry = chemistry,
#'                      q = q,
#'                      q_type = 'discharge',
#'                      method = c('beale', 'pw'))

ms_calc_flux_rsfme <- function(chemistry, 
                               q, 
                               method = 'simple',
                               aggregation = NULL, 
                               verbose = TRUE,
                               good_year_check = TRUE){
    
    library('dplyr', quietly = TRUE)
    
    #checks
    if(! all(c('site_code', 'val', 'var', 'date', 'ms_interp', 'ms_status') %in% names(chemistry))){
        stop('The argument to `chemistry` must be in MacroSheds format (required columns: date, site_code, val, var, ms_interp, ms_status).')
    }
    if(! all(c('site_code', 'val', 'var', 'date', 'ms_interp', 'ms_status') %in% names(q))){
        stop('The argument to `q` must be in MacroSheds format (required columns: date, site_code, val, var, ms_interp, ms_status).')
    }
    
    q_type <- unique(q$var)
    if(length(q_type) > 1){
        stop('q$var` may not contain more than 1 unique value')
    }
    if(! q_type %in% c('discharge', 'precipitation')){
        stop('`q$var` must be entirely "discharge" or "precipitation".')
    }
    
    if(q_type == 'precipitation' & any(method != 'simple')){
        stop('Only method = "simple" is appropriate for precipitation data.')
    }
    
    if(! inherits(q$date, 'Date')){
        q$date <- as.Date(q$date)
    }
    if(! inherits(chemistry$date, 'Date')){
        chemistry$date <- as.Date(chemistry$date)
    }
    
    requireNamespace('macrosheds', quietly = TRUE)
    
    # make sure method is accepted, and if method is 'rsfme' set method to all rsfme methods
    # check that method, if non-null, is in accepted list
    rsfme_accepted  <- c('average', 'pw', 'composite', 'beale', 'rating', 'simple', 'rsfme')
    rsfme_methods  <- c('average', 'pw', 'composite', 'beale', 'rating')
    
    if('rsfme' %in% method){
        method <- rsfme_methods
    }
    
    if(! all(method %in% rsfme_accepted)){
        stop(glue::glue('Unrecognized flux method: {setdiff(method, rsfme_accepted)}'))
    } else {
        if(verbose){
            cat(glue::glue('calculating flux using method(s): {m}',
                           m = paste(method, collapse = ', ')))
        }
    }
    
    riverload_methods <- c('pw', 'beale', 'rating', 'composite')
    if(any(method %in% riverload_methods)){
        
        # look for RiverLoad package on user machine
        rl.res <- try(find.package('RiverLoad'), silent = TRUE)
        
        # if not found, stop and give address for download
        if(inherits(rl.res, 'try-error')){
            stop('package "RiverLoad" required for methods: pw, beale, rating, and composite. ',
                 'Install with:\n\tremotes::install_github("https://github.com/cran/RiverLoad.git")')
        }
    }
    
    # make sure agg option is annual or monthly if calculating any non-null method
    # and otherwise timestep is data-res and using simple QC
    rsfme_aggs <- c('annual', 'monthly')
    
    if(! is.null(aggregation) & length(aggregation) != 1){
        stop(glue::glue('If using any method other than "simple", choose *only one* aggregation from the following:\n {list}',
                        list = paste0(rsfme_aggs, collapse = ', ')))
    }
    
    if(any(method == 'simple')){
        
        if(length(method) > 1){
            warning('method = "simple" detected, so ignoring other methods. ',
                    'Omit "simple" to use other flux methods.')
        }
        
        method <- 'simple'
        
        if(! is.null(aggregation)){
            if(verbose){
                cat('Ignoring "aggregation" parameter because method = "simple".')
            }
        }
        
        aggregation <- 'simple'
        
    } else {
        
        if(! aggregation %in% rsfme_aggs){
            stop(glue::glue('"aggregation" must be one of: {paste0(rsfme_aggs, collapse = ", ")}'))
        }
    }
    
    # pull in variable data
    var_info <- macrosheds::ms_load_variables()
    
    # pull in MS site info
    site_info <- macrosheds::ms_load_sites()
    site_info$ws_area_ha <- errors::set_errors(site_info$ws_area_ha, 0)
    
    # verify that both files have the same sites
    sites_chem <- unique(chemistry$site_code)
    sites <- sites_chem
    sites_q <- unique(q$site_code)
    
    if(! setequal(sites_chem, sites_q)){
        stop('"chemistry" and "q" must have the same sites.')
    }
    
    # # verify that the intervals are the same in both chemistry and q
    # q_interval <- Mode(diff(as.numeric(q$date)))
    # 
    # interval <- case_when(q_interval == 86400 ~ 'daily',
    #                       q_interval == 3600 ~ 'hourly',
    #                       q_interval == 1800 ~ '30 minute',
    #                       q_interval == 960 ~ '15 minute',
    #                       q_interval == 600 ~ '10 minute',
    #                       q_interval == 300 ~ '5 minute',
    #                       q_interval == 60 ~ '1 minute')
    # 
    # if(is.na(interval)){
    #     stop(paste0('interval of samples must be one',
    #                 ' of: daily, hourly, 30-minute, 15-minute, 10-minute, 5-minute, or 1-minute.',
    #                 ' See ms_synchronize_timestep() to standardize your intervals.'))
    # } else if(verbose){
    #     print(paste0('Input `q` dataset has a ', interval, ' interval.'))
    # }
    
    # add errors if they don't exist
    if('val_err' %in% colnames(chemistry)){
        
        errors::errors(chemistry$val) <- chemistry$val_err
        chemistry <- dplyr::select(chemistry, -val_err)
        
    } else if(! inherits(chemistry$val, 'errors')){
        errors::errors(chemistry$val) <- 0
    }
    
    if('val_err' %in% colnames(q)){
        
        errors::errors(q$val) <- q$val_err
        q <- dplyr::select(q, -val_err)
        
    } else if(! inherits(q$val, 'errors')){
        errors::errors(q$val) <- 0
    }
    
    # calc flux 'simple' dataframe
    all_sites_flux <- tibble()
    
    # calc flux 'rsfme' dataframe, to populate with annual flux values by method
    out_frame <- tibble(wy = as.integer(),
                        site_code = as.character(),
                        var = as.character(),
                        val = as.numeric(),
                        method = as.character(),
                        ms_reccomended = as.integer(),
                        ms_interp_ratio = as.numeric(),
                        ms_status_ratio = as.numeric(),
                        ms_missing_ratio = as.numeric())
    
    if(aggregation == 'monthly'){
        
        out_frame <- tibble(wy = as.integer(),
                            month = as.integer(),
                            site_code = as.character(),
                            var = as.character(),
                            val = as.numeric(),
                            method = as.character(),
                            ms_reccomended = as.integer(),
                            ms_interp_ratio = as.numeric(),
                            ms_status_ratio = as.numeric(),
                            ms_missing_ratio = as.numeric())
    }
    
    for(s in 1:length(sites)){
        
        site_code <- sites[s]
        
        # filter q and chem to just this site for calcs
        site_chem <- chemistry %>%
            filter(site_code == !!site_code)
        
        # get daterange of chem dataset and filter q to match
        daterange <- as.Date(range(site_chem$date))
        site_q <- q %>%
            filter(site_code == !!site_code,
                   date >= !!daterange[1] - 14, # two weeks before chem
                   date <= !!daterange[2] + 14) # two weeks after chem
        
        if(nrow(site_q) == 0){
            warning(glue::glue('Site {site_code} has no Q-C overlap; skipping.'))
            next
        }
        
        this_site_info <- site_info %>% 
            filter(site_code == !!site_code,
                   site_type == 'stream_gauge') %>% 
            distinct()
        
        area <- this_site_info$ws_area_ha
        lat <- this_site_info$latitude
        long <- this_site_info$longitude
        
        errors::errors(area) <- 0
        
        chem_split <- site_chem %>%
            group_by(var) %>%
            arrange(date) %>%
            dplyr::group_split() %>%
            as.list()
        
        # loop though all variables
        for(i in 1:length(chem_split)){
            
            chem_chunk <- chem_split[[i]]
            raw_data_con_in <- chem_chunk
            
            # check var is "flux convertible"
            this_var <- ms_drop_var_prefix(unique(chem_chunk$var))
            
            this_var_info <- var_info %>%
                filter(variable_code == this_var)
            
            if(any(this_var_info$flux_convertible == 0)){
                if(verbose){
                    warning(glue::glue('Cannot determine flux-convertability of {v}; skipping', v = this_var))
                }
                next
            }
            
            # 'simple' flux method identifies highest possible resolution and performs Q*C
            # calc on this data
            if('simple' %in% method){
                # join Q and Chem data
                chem_is_highres <- Mode(diff(as.numeric(chem_chunk$datetime))) <= 15 * 60
                if(is.na(chem_is_highres)) chem_is_highres <- FALSE
                
                #if both chem and flow data are low resolution (grab samples),
                #   let approxjoin_datetime match up samples with a 12-hour gap. otherwise the
                #   gap should be 7.5 mins so that there isn't enormous duplication of
                #   timestamps where multiple high-res values can be snapped to the
                #   same low-res value
                flow_is_highres <- q_interval <= 15 * 60
                if(is.na(flow_is_highres)) flow_is_highres <- FALSE
                if(! chem_is_highres && ! flow_is_highres){
                    join_distance <- c('12:00:00')#, '%H:%M:%S')
                } else {
                    join_distance <- c('7:30')#, '%M:%S')
                }
                
                # create unified dataframe of this solute
                chem_split[[i]] <- approxjoin_datetime(x = chem_chunk,
                                                       y = site_q,
                                                       rollmax = join_distance,
                                                       keep_datetimes_from = 'x')
                # calculate simple flux
                if(q_type == 'discharge'){
                    chem_split[[i]] <- chem_split[[i]] %>%
                        mutate(site_code = site_code_x,
                               var = var_x,
                               # kg/interval = mg/L *  L/s  * q_interval / 1e6
                               val = val_x * val_y * errors::as.errors(q_interval) / errors::as.errors(1e6),
                               ms_status = numeric_any_v(ms_status_x, ms_status_y),
                               ms_interp = numeric_any_v(ms_interp_x, ms_interp_y)) %>%
                        dplyr::select(-starts_with(c('site_code_', 'var_', 'val_',
                                                     'ms_status_', 'ms_interp_'))) %>%
                        filter(! is.na(val)) %>% #should be redundant
                        arrange(datetime) %>%
                        ms_scale_flux_by_area()
                } else {
                    chem_split[[i]] <- chem_split[[i]] %>%
                        mutate(site_code = site_code_x,
                               var = var_x,
                               # kg/interval/ha = mg/L *  mm/interval * ha/100
                               val = val_x * val_y / errors::as.errors(100),
                               ms_status = numeric_any_v(ms_status_x, ms_status_y),
                               ms_interp = numeric_any_v(ms_interp_x, ms_interp_y)) %>%
                        dplyr::select(-starts_with(c('site_code_', 'var_', 'val_',
                                                     'ms_status_', 'ms_interp_'))) %>%
                        filter(! is.na(val)) %>% #should be redundant
                        arrange(datetime)
                }
                
                flux <- chem_split %>%
                    purrr::reduce(bind_rows) %>%
                    arrange(site_code, var, datetime)
                
                all_sites_flux <- rbind(all_sites_flux, flux)
                
            } else {
                # RSFME methods
                if(any(method %in% rsfme_accepted)){
                    # check chem dataframe for data minimums
                    raw_data_con <- chem_chunk %>%
                        # only original data, greater than zero
                        filter(ms_interp == 0, val > 0) %>%
                        dplyr::select(datetime, val) %>%
                        tidyr::drop_na(datetime, val)
                    
                    chunk_daterange <- range(raw_data_con$datetime)
                    
                    raw_data_q <- site_q %>%
                        filter(
                            datetime >= !!chunk_daterange[1],
                            datetime <= !!chunk_daterange[2]
                        )
                    
                    # check for "good years"
                    if(good_year_check == TRUE){
                        
                        # find acceptable years
                        q_check <- raw_data_q %>%
                            mutate(date = lubridate::date(datetime)) %>%
                            filter(ms_interp == 0) %>%
                            distinct(date, .keep_all = TRUE) %>%
                            mutate(water_year = wtr_yr(datetime, start_month = 10)) %>%
                            group_by(water_year) %>%
                            summarise(n = n()) %>%
                            ungroup() %>%
                            filter(n >= 311)
                        
                        conc_check <- raw_data_con %>%
                            mutate(date = lubridate::date(datetime)) %>%
                            distinct(., date, .keep_all = TRUE) %>%
                            mutate(water_year = wtr_yr(date, start_month = 10),
                                   quart = lubridate::quarter(date)) %>%
                            group_by(water_year) %>%
                            summarise(count = n_distinct(quart),
                                      n = n()) %>%
                            ungroup() %>%
                            filter(n >= 4,
                                   count > 3)
                        
                        q_good_years <- q_check$water_year
                        conc_good_years <- conc_check$water_year
                        
                        # 'good years' where Q and Chem data both meet min requirements
                        good_years <- q_good_years[q_good_years %in% conc_good_years]
                        n_yrs <- length(good_years)
                        
                    } # good year check
                    
                    if(n_yrs == 0){
                        warning(glue::glue('Variable {v} has no years of sufficient overlap of chemistry and discharge ',
                                           'to perform flux calculations.', v = this_var))
                        next
                    }
                    
                    # join data and cut to good years
                    daily_data_con <- raw_data_con %>%
                        mutate(date = lubridate::date(datetime)) %>%
                        group_by(date) %>%
                        summarize(val = mean_or_x(val)) %>%
                        ungroup() %>%
                        # this is the step where concentration value errors turn to NA
                        mutate(site_code = !!site_code, var = 'con') %>%
                        dplyr::select(site_code, datetime = date, var, val)
                    
                    daily_data_q <- raw_data_q %>%
                        mutate(date = lubridate::date(datetime)) %>%
                        group_by(date) %>%
                        summarize(val = mean_or_x(val)) %>%
                        ungroup() %>%
                        # this is the step where discharge value errors turn to NA
                        mutate(site_code = !!site_code, var = 'q_lps') %>%
                        dplyr::select(site_code, datetime = date, var, val)
                    
                    q_df <- daily_data_q %>%
                        tidyr::pivot_wider(names_from = var,
                                           values_from = val)
                    
                    raw_data_full <- rbind(daily_data_con, daily_data_q) %>%
                        tidyr::pivot_wider(names_from = var, values_from = val,
                                           id_cols = c(site_code, datetime)) %>%
                        mutate(wy = wtr_yr(datetime, start_month = 10),
                               month = lubridate::month(datetime)) %>%
                        filter(wy %in% good_years) %>%
                        ungroup()
                    
                    for(k in 1:n_yrs){
                        
                        target_year <- as.numeric(as.character(good_years[k]))
                        target_solute <- this_var
                        
                        # set "period" for other flux calc args
                        if(aggregation == 'monthly'){
                            period <- 'month'
                        } else if(aggregation == 'annual'){
                            period <- 'annual'
                        } else {
                            stop('invalid aggregation for advanced flux estimates',
                                 'aggregation must be "annual" or "monthly"')
                        }
                        
                        if(aggregation == 'monthly'){
                            # calculate flag ratios to carry forward
                            flag_df <- carry_flags(raw_q_df = raw_data_q,
                                                   raw_con_df = raw_data_con_in,
                                                   target_year = target_year,
                                                   target_solute = target_solute,
                                                   period = period)
                        } else {
                            # calculate flag ratios to carry forward
                            flag_df <- carry_flags(raw_q_df = raw_data_q,
                                                   raw_con_df = raw_data_con_in,
                                                   target_year = target_year,
                                                   target_solute = target_solute,
                                                   period = period)
                        }
                        
                        raw_data_target_year <- raw_data_full %>%
                            mutate(wy = as.numeric(as.character(wy))) %>%
                            filter(wy == !!target_year)
                        
                        q_target_year <- raw_data_target_year %>%
                            dplyr::select(site_code, datetime, q_lps, wy) %>%
                            na.omit()
                        
                        con_target_year <- raw_data_target_year %>%
                            dplyr::select(site_code, datetime, con, wy) %>%
                            na.omit()
                        
                        ### calculate annual flux ######
                        chem_df_errors <- con_target_year
                        q_df_errors <- q_target_year
                        
                        ### save and then remove errors attribute for calcs
                        chem_df <- errors::drop_errors(chem_df_errors)
                        q_df <- errors::drop_errors(q_df_errors)
                        
                        ### add month column, for monthly agg use
                        chem_df <- chem_df %>%
                            mutate(month = lubridate::month(datetime))
                        q_df <- q_df %>%
                            mutate(month = lubridate::month(datetime))
                        # browser()
                        
                        # set up dummy flux values to be replaced if user has chosen to run method
                        if(aggregation == 'annual'){
                            flux_annual_average = NA
                            flux_annual_pw = NA
                            flux_annual_beale = NA
                            flux_annual_rating = NA
                            flux_annual_comp = NA
                        } else {
                            flux_monthly_average = c()
                            flux_monthly_pw = c()
                            flux_monthly_beale = c()
                            flux_monthly_rating = c()
                            flux_monthly_comp = c()
                            
                            flux_monthly_average$flux =    rep(NA, 12)
                            flux_monthly_pw$flux      =    rep(NA, 12) 
                            flux_monthly_beale$flux   =    rep(NA, 12) 
                            flux_monthly_rating$flux  =    rep(NA, 12) 
                            flux_monthly_comp$flux    =    rep(NA, 12) 
                        }
                        
                        # will need: devtools::install_github('https://github.com/cran/RiverLoad.git')
                        
                        if('average' %in% method){
                            #### calculate average ####
                            if(aggregation == 'monthly'){
                                flux_monthly_average <- sw(raw_data_target_year %>%
                                                               group_by(wy, month) %>%
                                                               summarize(q_lps = mean(q_lps, na.rm = TRUE),
                                                                         con = mean(con, na.rm = TRUE), .groups = 'drop_last') %>%
                                                               ungroup() %>%
                                                               # multiply by seconds in a year, and divide by mg to kg conversion (1M)
                                                               mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
                                                               select(month, flux))
                            } else {
                                flux_annual_average <- sw(raw_data_target_year %>%
                                                              group_by(wy) %>%
                                                              summarize(q_lps = mean(q_lps, na.rm = TRUE),
                                                                        con = mean(con, na.rm = TRUE)) %>%
                                                              ungroup() %>%
                                                              # multiply by seconds in a year, and divide by mg to kg conversion (1M)
                                                              mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
                                                              pull(flux))
                            }
                        }
                        
                        #### calculate period weighted #####
                        if(aggregation == 'annual'){
                            if('pw' %in% method){
                                flux_annual_pw <- calculate_pw(chem_df, q_df, datecol = 'datetime',
                                                               area = area, period = period)
                            }
                        } else {
                            # NOTE: for now, month-order is based off of pw, so this needs to run 
                            # for monthly flux calcs internals to run (should be fixed)
                            flux_annual_pw <- calculate_pw(chem_df, q_df, datecol = 'datetime',
                                                           area = area, period = period)
                        }
                        
                        #### calculate beale ######
                        if('beale' %in% method){
                            flux_annual_beale <- calculate_beale(chem_df, q_df, datecol = 'datetime',
                                                                 area = area, period = period)
                        }
                        #### calculate rating #####
                        if('rating' %in% method){
                            flux_annual_rating <- calculate_rating(chem_df, q_df, datecol = 'datetime',
                                                                   area = area, period = period)
                        }
                        
                        #### calculate composite ######
                        if('composite' %in% method){
                            rating_filled_df <- generate_residual_corrected_con(chem_df = chem_df,
                                                                                q_df = q_df,
                                                                                datecol = 'datetime',
                                                                                sitecol = 'site_code')
                            # calculate annual flux from composite
                            flux_annual_comp <- calculate_composite_from_rating_filled_df(rating_filled_df,
                                                                                          area = area,
                                                                                          period = period)
                            flux_comp_dq <- FALSE
                            if(any(flux_annual_comp < 0)){
                                flux_comp_dq <- TRUE
                            }
                        }
                        
                        if(period == 'month'){
                            # NOTE: lots of code with month matching and merging below this is all 
                            # to match everything to month order of period weighted, to account for 
                            # possible worlds where water year rearranges month order, etc.
                            months <- sapply(strsplit(flux_annual_pw$date, '-'), `[`, 2)
                            flux_monthly_pw <- flux_annual_pw %>%
                                mutate(month = months) %>%
                                select(month, flux)
                            
                            if(length(months) != 12){
                                warning('NOTE: number of months in pw not 12, need handling for setting NA to uncalc months')
                            }
                            
                            # beale
                            if('beale' %in% method){
                                months_beale <- sapply(strsplit(flux_annual_beale$date, '-'), `[`, 2)
                                flux_monthly_beale <- flux_annual_beale %>%
                                    mutate(month = as.character(months_beale))
                                flux_monthly_beale <- flux_monthly_beale[match(months, flux_monthly_beale$month),]
                                flux_monthly_beale <- left_join(flux_monthly_pw %>% select(month),
                                                                flux_monthly_beale, by = 'month') %>%
                                    select(month, flux)
                            }
                            
                            # rating
                            if('rating' %in% method){
                                months_rating <- sapply(strsplit(flux_annual_rating$date, '-'), `[`, 2)
                                flux_monthly_rating <- flux_annual_rating %>%
                                    mutate(month = as.character(months_rating))
                                flux_monthly_rating <- flux_monthly_rating[match(months, flux_monthly_rating$month),]
                                flux_monthly_rating <- left_join(flux_monthly_pw %>% select(month),
                                                                 flux_monthly_rating, by = 'month') %>%
                                    select(month, flux)
                            }
                            
                            # comp
                            if('composite' %in% method){
                                flux_monthly_comp <- flux_annual_comp[match(as.double(months), flux_annual_comp$month),]
                                flux_monthly_comp <- flux_monthly_comp %>%
                                    mutate(month = sprintf('%02d', month))
                                # make sure all months present, filled w NAs if no value
                                flux_monthly_comp <- left_join(flux_monthly_pw %>% select(month),
                                                               flux_monthly_comp, by = 'month') %>%
                                    select(month, flux)
                            }
                            
                            if(!'pw' %in% method){
                                flux_monthly_pw$flux <- rep(NA, 12)
                            }
                            
                        }
                        
                        #### select MS favored ####
                        if(period == 'month'){
                            paired_df <- q_df %>%
                                full_join(chem_df, by = c('datetime', 'site_code', 'wy', 'month'))
                        } else {
                            paired_df <- q_df %>%
                                full_join(chem_df, by = c('datetime', 'site_code', 'wy'))
                        }
                        
                        paired_df <- na.omit(paired_df) %>% 
                            filter(
                                # no negative flow
                                q_lps > 0,
                                is.finite(q_lps),
                                # no negative concentration
                                con > 0,
                                is.finite(con))
                        
                        model_data <- tibble(c_log = log10(paired_df$con),
                                             q_log = log10(paired_df$q_lps))
                        
                        # ``model_data`` is the site-variable-year dataframe of Q and concentration
                        # log-log rating curve of C by Q
                        rating <- summary(lm(model_data$c_log ~ model_data$q_log, singular.ok = TRUE))
                        # R^2 value of rating curve
                        r_squared <- rating$r.squared
                        # auto-correlation of residuals of rating curve
                        resid_acf <- abs(acf(rating$residuals, lag.max = 1, plot = FALSE)$acf[2])
                        # auto-correlation of concentration data
                        con_acf <- abs(acf(paired_df$con, lag.max = 1, plot = FALSE)$acf[2])
                        
                        # modified from figure 10 of Aulenbach et al 2016
                        if(!is.nan(r_squared)){
                            if(r_squared > 0.3){
                                if(resid_acf > 0.2){
                                    recommended_method <- 'composite'
                                }else{
                                    recommended_method <- 'rating'
                                }
                            }else{
                                if(con_acf > 0.20){
                                    recommended_method <- 'pw'
                                } else {
                                    recommended_method <- 'average'
                                }
                            }
                        } else {
                            if(verbose){
                                warning(glue::glue('recommended method warning:  {site_code}, {target_solute}, {target_year}\n',
                                                   'during application of Aulenbach et al. 2016 procedure for choosing reccomended load estimation method',
                                                   ' concentration:discharge log-log linear regression r_squared value was NaN; recommended method set to NA\n\n'))
                            }
                            recommended_method <- NA
                        }
                        
                        if(exists('flux_comp_dq')){
                            if(recommended_method == 'composite' & flux_comp_dq){
                                recommended_method <- 'period weighted'
                                
                                if(verbose){
                                    warning(glue::glue('recommended method warning:  {site_code}, {target_solute}, {target_year}\n',
                                                       'the recommended method was set to composite',
                                                       ' using procedure from Aulenbach et al. 2016, but that method results in illegal values.',
                                                       ' Setting reccomended method to period-weighting instead.'))
                                }
                            }
                        }
                        
                        #### congeal fluxes ####
                        if(period == 'month'){
                            target_year_out <- tibble(
                                wy = as.character(target_year),
                                month = rep(months, 5),
                                val = c(flux_monthly_average$flux,
                                        flux_monthly_pw$flux,
                                        flux_monthly_beale$flux,
                                        flux_monthly_rating$flux,
                                        ## flux_monthly_wrtds,
                                        flux_monthly_comp$flux),
                                site_code = !!site_code,
                                var = !!target_solute,
                                method = c(rep('average', 12), rep('pw', 12), rep('beale', 12),
                                           rep('rating', 12), rep('composite', 12))
                            ) %>%
                                mutate(ms_recommended = ifelse(method == !!recommended_method, 1, 0))
                        } else {
                            # restructure flux annual comp to look like others in outframe creation
                            if(length(flux_annual_comp) > 1){
                                flux_annual_comp <- flux_annual_comp$flux[1]
                            }
                            target_year_out <- tibble(wy = as.character(target_year),
                                                      val = c(flux_annual_average,
                                                              flux_annual_pw,
                                                              flux_annual_beale,
                                                              flux_annual_rating,
                                                              ## flux_annual_wrtds,
                                                              flux_annual_comp),
                                                      site_code = !!site_code,
                                                      var = !!target_solute,
                                                      method = c('average', 'pw', 'beale', 'rating', 'composite')) %>%
                                mutate(ms_recommended = ifelse(method == !!recommended_method, 1, 0))
                        }
                        
                        out_frame <- rbind(out_frame, target_year_out)
                        
                    } # good years
                    
                } # if any rsfme methods
                
            } # if not simple
        } # end variables loop
    } # end sites loop
    
    if(any(method == 'simple')){
        if(nrow(all_sites_flux) == 0) return()
        
        all_sites_flux$val_err <- errors::errors(all_sites_flux$val)
        all_sites_flux$val <- errors::drop_errors(all_sites_flux$val)
        
        return(all_sites_flux)
    } else {
        # filter to requested method
        out_frame <- out_frame %>%
            filter(method %in% !!method) %>%
            # set negative or infinite flux vals in final product to zero
            mutate(val = ifelse(val < 0, 0, val))
        
        return(out_frame)
    } # method return for simple or rsfme
    
    # TODO: make logic of simple vs RSFME clearer and articulate
    # TODO: make this work for *any* data in MacroSheds format
    # TODO: WRTDS inclusion
    # TODO: log file option
    # TODO: make handling and make clear that RSFME methods are for q_type discharge only
}
