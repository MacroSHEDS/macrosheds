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
            cat(glue::glue('calculating flux using method(s): {m}\n',
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
    
    #validate aggregation and method parameters
    if(any(method == 'simple')){
        
        if(length(method) > 1){
            warning('method = "simple" detected, so ignoring other methods. ',
                    'Omit "simple" to use other flux methods.')
        }
        method <- 'simple'
        
        if(! is.null(aggregation)){
            if(verbose){
                cat('Ignoring "aggregation" parameter because method = "simple".\n')
            }
        }
        aggregation <- 'simple'
        
    } else {
        
        if(is.null(aggregation) || length(aggregation) != 1 || ! aggregation %in% c('annual', 'monthly')){
            stop('Unless method = "simple", aggregation must be either "annual" or "monthly"')
        }
    }
    
    var_info <- macrosheds::ms_load_variables()
    site_info <- macrosheds::ms_load_sites()
    
    # verify that both files have the same sites
    sites_chem <- unique(chemistry$site_code)
    sites <- sites_chem
    sites_q <- unique(q$site_code)
    
    if(! setequal(sites_chem, sites_q)){
        stop('"chemistry" and "q" must have the same sites.')
    }
    
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
                        ms_recommended = as.integer(),
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
                            ms_recommended = as.integer(),
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
        # area <- errors::set_errors(this_site_info$ws_area_ha, 0)
        
        site_chem_split <- site_chem %>%
            group_by(var) %>%
            arrange(date) %>%
            dplyr::group_split() %>%
            as.list()
        
        for(i in seq_along(site_chem_split)){
            
            chem_var <- site_chem_split[[i]]
            
            fluxable <- var_info %>%
                filter(variable_code == !!ms_drop_var_prefix(unique(chem_var$var))) %>% 
                pull(flux_convertible)
            
            if(any(fluxable == 0)){
                warning(glue::glue('Flux methods not yet defined for {this_var}; skipping'))
                next
            }
            
            if(any(method == 'simple')){
                
                site_chem_split[[i]] <- calc_simple_flux(chem = chem_var,
                                                         q = site_q)
                
            } else {
                
                calc_load(chem = chem_var,
                          q = site_q,
                          site_code = site_code,
                          area = area,
                          method = method,
                          aggregation = aggregation,
                          good_year_check = good_year_check)
            }
        }
        
        all_sites_flux <- site_chem_split %>%
            purrr::reduce(bind_rows) %>%
            arrange(site_code, var, date) %>% 
            bind_rows(all_sites_flux)
    }
    
    if(any(method == 'simple')){
        
        if(nrow(all_sites_flux) > 0){
            all_sites_flux$val_err <- errors::errors(all_sites_flux$val)
            all_sites_flux$val <- errors::drop_errors(all_sites_flux$val)
        }
        
        return(all_sites_flux)
        
    } else {
        
        # filter to requested method
        out_frame <- out_frame %>%
            filter(method %in% !!method) %>%
            # set negative or infinite flux vals in final product to zero
            mutate(val = if_else(val < 0, 0, val))
        
        return(out_frame)
    }
    
    # TODO: make logic of simple vs RSFME clearer and articulate
    # TODO: make this work for *any* data in MacroSheds format
    # TODO: WRTDS inclusion
    # TODO: log file option
    # TODO: make clear that RSFME methods are for q_type discharge only
}
