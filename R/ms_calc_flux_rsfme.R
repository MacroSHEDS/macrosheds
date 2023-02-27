#' Calculates chemical fluxes
#'
#' Calculates solute fluxes from Q (discharge
#' or precipitation) and chemistry data.
#'
#' @author Wes Slaughter, \email{wslaughter@@berkeley.edu}
#' @author Nick Gubbins, \email{gubbinsnick@@gmail.com}
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#'
#' @param chemistry \code{data.frame}. A \code{data.frame} of precipitation or
#'    stream chemistry data in MacroSheds format and in units of mg/L.
#' @param q \code{data.frame}. A \code{data.frame} of precipitation or stream
#'    discharge in MacroSheds format and in units of mm or L/s, respectively.
#' @param q_type character. Either 'precipitation' or 'discharge'.
#' @param verbose logical. Default TRUE; prints more information to console.
#' @return returns a \code{tibble} of stream or precipitation chemical flux for every timestep
#'    where discharge/precipitation and chemistry are reported. Output units are kg/ha/timestep.
#' @details
#' Chemical flux is calculated by multiplying chemical concentration by flow
#' of water (flux = concentration * flow). The output units depend on the time
#' interval at which input data are collected. The resulting flux units will always be
#' kg/ha/T, where T is the time interval of the input \code{tibble}. \code{q_type} is used
#' to calculate flux differently because of the different units of discharge and precipitation.
#' If \code{q_type} is 'discharge', flux is calculated as: kg/ha/T = mg/L * L/s * T / 1e6 / ws_area.
#' If \code{q_type} is 'precipitation', is calculated as: kg/ha/T (kg/ha/T = mg/L * mm/T / 100).
#' You can convert between kg/ha/T and kg/T using [ms_scale_flux_by_area()] and
#' [ms_undo_scale_flux_by_area()].
#'
#' Before running [ms_calc_flux()], ensure both \code{q} and
#' \code{chemistry} have the same time interval. See [ms_synchronize_timestep()].
#' Also ensure chemistry units are mg/L. See [ms_conversions()].
#' @seealso [ms_synchronize_timestep()], [ms_conversions()], [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @export
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

ms_calc_flux_rsfme <- function(chemistry, q, q_type, verbose = TRUE, method = c('average', 'beale', 'pw', 'rating', 'composite'), aggregation = 'annual', good_year_check = TRUE) {

    library("dplyr", quietly = TRUE)

    #### Checks
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(chemistry))){
        stop('The argument to chemistry must contain precipitation chemistry or stream chemistry data in MacroSheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum).')
    }
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(q))){
        stop('The argument to q must contain precipitation or stream discharge data in MacroSheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum).')
    }

    if(! grepl('(precipitation|discharge)', q_type)){
        stop('q_type must be "discharge" or "precipitation for rsfme flux methods"')
    }

    if(q_type == 'precipitation' & any(!!method != "simple")) {
      warning('setting flux calculation method to "simple," as RSFME methods are intended only for',
              'surface runoff solute flux estimation.')
    }

    available_aggs <- c('annual', 'monthly')
    if(!aggregation %in% c('annual', 'monthly')) {
      stop('aggregation must be "annual" OR "monthly"')
    }

    if(! 'POSIXct' %in% class(q$datetime)){
        q$datetime <- as.POSIXct(q$datetime)
    }
    if(! 'POSIXct' %in% class(chemistry$datetime)){
        chemistry$datetime <- as.POSIXct(chemistry$datetime)
    }

    requireNamespace('macrosheds', quietly = TRUE)

    # make sure method is accepted, and if method is 'rsfme' set method to all rsfme methods
    # check that method, if non-null, is in accepted list
    rsfme_accepted  <- c('average', 'pw', 'composite', 'beale', 'rating', 'simple', 'rsfme')
    rsfme_methods  <- c('average', 'pw', 'composite', 'beale', 'rating')

    if('rsfme' %in% method) {
      method <- rsfme_methods
    }

    if(!all(method %in% rsfme_accepted)) {
      stop(glue::glue('at least one flux calculation method supplied is not in accepted list, must be one of the following:\n {list}',
                list = rsfme_accepted))
    } else {
      writeLines(glue::glue('calculating flux using method(s): {method}', method = list(method)))
    }

    riverload_methods <- c('pw', 'beale', 'rating', 'composite')
    if(any(method %in% riverload_methods)) {
      # look for RiverLoad package on user machine
      rl.res <- try(find.package('RiverLoad'))
      # if not found, stop and give address for download
      if(class(rl.res) == 'try-error'){
        stop('package RiverLoad required for pw, beale, rating, and composite flux estimation methods.\n',
                'install using devtools::install_github("https://github.com/cran/RiverLoad.git") or\n',
                'remotes::install_github("https://github.com/cran/RiverLoad.git")')
      }
    }

    # make sure agg option is annual or monthly if calculating any non-null method
    # and otherwise timestep is data-res and using simple QC
    rsfme_aggs <- c('annual', 'monthly', 'simple')
    if(!aggregation %in% rsfme_aggs) {
      stop(glue::glue('time aggregation is not in accepted list, must be one of the following:\n {list}',
                list = rsfme_aggs))
    } else if(aggregation == 'simple') {
      writeLines(glue::glue('aggregating flux at highest possible resolution timestep of data supplied, using simple Q*C methods', aggregation = aggregation))
    } else {
      writeLines(glue::glue('aggregating flux over: {aggregation}', aggregation = aggregation))
    }

    ## if(aggregation == 'monthly') {
    ##   stop('monthly aggregation currently unavailable with ms_calc_flux_rsfme(), only "annual" and "simple" flux calcs')
    ## }

    # pull in variable data
    var_info <- macrosheds::ms_load_variables()

    # pull in MS site info
    site_info <- macrosheds::ms_site_data
    site_info$ws_area_ha <- errors::set_errors(site_info$ws_area_ha, 0)

    # Check both files have the same sites
    sites_chem <- unique(chemistry$site_code)
    sites_q <- unique(q$site_code)

    if(! all(sites_chem %in% sites_q)){
        stop('Both chemistry and q must have the same sites')
    }

    sites <- sites_chem

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
        stop(paste0('interval of samples must be one',
                    ' of: daily, hourly, 30 minute, 15 minute, 10 minute, 5 minute, or 1 minute.',
                    ' See macrosheds::ms_synchronize_timestep() to standardize your intervals.'))
    } else if(verbose) {
        print(paste0('input q dataset has a ', interval, ' interval'))
    }

    # add errors if they don't exist
    if('val_err' %in% names(chemistry)){
        errors::errors(chemistry$val) <- chemistry$val_err

        chemistry <- chemistry %>%
            dplyr::select(-val_err)

    } else if(all(errors::errors(chemistry$val) == 0)){
        errors::errors(chemistry$val) <- 0
    }

    if('val_err' %in% names(q)){
        errors::errors(q$val) <- q$val_err

        q <- q %>%
            dplyr::select(-val_err)

    } else if(all(errors::errors(q$val) == 0)){
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

    if(aggregation == "monthly") {
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

    for(s in 1:length(sites)) {

        site <- sites[s]
        site_code <- sites[s]

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

        site_info <- macrosheds::ms_load_sites()

        area <- site_info %>%
            filter(site_code == !!site_code) %>%
            distinct() %>%
            pull(ws_area_ha)

        lat <- site_info %>%
            filter(site_code == !!site_code) %>%
            distinct() %>%
            pull(latitude)

        long <- site_info %>%
            filter(site_code == !!site_code) %>%
            distinct() %>%
            pull(longitude)

        chem_split <- site_chem %>%
            group_by(var) %>%
            arrange(datetime) %>%
            dplyr::group_split() %>%
            as.list()

        # Loop though all variables
        for(i in 1:length(chem_split)) {

          chem_chunk <- chem_split[[i]]
          raw_data_con_in <- chem_chunk

          # check var is "flux convetable"
          this_var <- chem_chunk %>%
            pull(var) %>%
            unique() %>%
            ms_drop_var_prefix()

          this_var_info <- var_info %>%
            filter(variable_code == this_var)

          if(any(this_var_info$flux_convertible == 0)) {
            if(verbose) {
              warning(glue::glue('{v} is not flux convertible, skipping', v = this_var))
            }
            next
          }


          # 'simple' flux method identifies highest possible resolution and performs Q*C
          # calc on this data
          if('simple' %in% method) {
              # join Q and Chem data
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
            next
          } else {
            # RSFME methods
            if(any(method %in% rsfme_accepted)) {
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
              if(good_year_check == TRUE) {

                # find acceptable years
                q_check <- raw_data_q %>%
                  mutate(date = lubridate::date(datetime)) %>%
                    filter(ms_interp == 0) %>%
                    distinct(., date, .keep_all = TRUE) %>%
                    mutate(water_year = wtr_yr(datetime, start_month = 10)) %>%
                    group_by(water_year) %>%
                    summarise(n = n()) %>%
                    filter(n >= 311)

                conc_check <- raw_data_con %>%
                    mutate(date = lubridate::date(datetime)) %>%
                    distinct(., date, .keep_all = TRUE) %>%
                    mutate(water_year = wtr_yr(date, start_month = 10),
                           quart = lubridate::quarter(date)) %>%
                    group_by(water_year) %>%
                    summarise(count = n_distinct(quart),
                              n = n()) %>%
                    filter(n >= 4,
                           count > 3)

                q_good_years <- q_check$water_year
                conc_good_years <- conc_check$water_year

                # 'good years' where Q and Chem data both meet min requirements
                good_years <- q_good_years[q_good_years %in% conc_good_years]
                n_yrs <- length(good_years)

              } # good year check

              if(n_yrs == 0) {
                warning(glue::glue('variable {v} has no years of sufficient overlap of chemistry and discharge',
                                   'to perform flux calculations', v = this_var))
                next
              }

              #join data and cut to good years
              daily_data_con <- raw_data_con %>%
                mutate(date = lubridate::date(datetime)) %>%
                group_by(date) %>%
                summarize(val = mean_or_x(val)) %>%
                # this is the step where concentration value errors turn to NA
                mutate(site_code = !!site_code, var = 'con') %>%
                dplyr::select(site_code, datetime = date, var, val)

              daily_data_q <- raw_data_q %>%
                  mutate(date = lubridate::date(datetime)) %>%
                  group_by(date) %>%
                  summarize(val = mean_or_x(val)) %>%
                # this is the step where discharge value errors turn to NA
                  mutate(site_code = !!site_code, var = 'q_lps') %>%
                  dplyr::select(site_code, datetime = date, var, val)

              q_df <- daily_data_q %>%
               tidyr::pivot_wider(names_from = var,
                            values_from = val)

              raw_data_full <- rbind(daily_data_con, daily_data_q) %>%
                  tidyr::pivot_wider(names_from = var, values_from = val, id_cols = c(site_code, datetime)) %>%
                  mutate(wy = wtr_yr(datetime, start_month = 10),
                         month = lubridate::month(datetime)) %>%
                  filter(wy %in% good_years)

            for(k in 1:n_yrs) {

              target_year <- as.numeric(as.character(good_years[k]))
              target_solute <- this_var

              # set "period" for other flux calcs argument
              if(aggregation == "monthly") {
                period <- 'month'
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
                period <- NULL
              }

              raw_data_target_year <- raw_data_full %>%
                  mutate(wy = as.numeric(as.character(wy))) %>%
                  filter(wy == target_year)

              q_target_year <- raw_data_target_year %>%
                  dplyr::select(site_code, datetime, q_lps, wy)%>%
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

              # will need: devtools::install_github('https://github.com/cran/RiverLoad.git')
              #### calculate average ####

              if(aggregation == "monthly") {
                flux_annual_average <- raw_data_target_year %>%
                  group_by(wy, month) %>%
                  summarize(q_lps = mean(q_lps, na.rm = TRUE),
                            con = mean(con, na.rm = TRUE)) %>%
                  # multiply by seconds in a year, and divide my mg to kg conversion (1M)
                  mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
                  select(month, flux)
                if(length(flux_annual_average) != 12) {
                  warning('number of motnhs not 12, need handling for setting NA to uncalc months')
                }
              } else {
                flux_annual_average <- raw_data_target_year %>%
                  group_by(wy) %>%
                  summarize(q_lps = mean(q_lps, na.rm = TRUE),
                            con = mean(con, na.rm = TRUE)) %>%
                  # multiply by seconds in a year, and divide my mg to kg conversion (1M)
                  mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
                  pull(flux)
              }


              #### calculate period weighted #####
              flux_annual_pw <- calculate_pw(chem_df, q_df, datecol = 'datetime',
                                             area = area, period = period)

              #### calculate beale ######
              flux_annual_beale <- calculate_beale(chem_df, q_df, datecol = 'datetime',
                                                   area = area, period = period)

              #### calculate rating #####
              flux_annual_rating <- calculate_rating(chem_df, q_df, datecol = 'datetime',
                                                     area = area, period = period)

              #### calculate composite ######
              rating_filled_df <- generate_residual_corrected_con(chem_df = chem_df,
                                                                  q_df = q_df,
                                                                  datecol = 'datetime',
                                                                  sitecol = 'site_code')

              # calculate annual flux from composite
              flux_annual_comp <- calculate_composite_from_rating_filled_df(rating_filled_df,
                                                                            area = area,
                                                                            period = period)

              #### select MS favored ####
              if(period == 'monthly') {
                paired_df <- q_df %>%
                  full_join(chem_df, by = c('datetime', 'site_code', 'wy', 'month')) %>%
                  na.omit() %>%
                filter(
                  # no negative flow
                  q_lps > 0,
                  is.finite(q_lps),
                  # no negative concentration
                  con > 0,
                  is.finite(con))

              } else {
                paired_df <- q_df %>%
                  full_join(chem_df, by = c('datetime', 'site_code', 'wy')) %>%
                  na.omit() %>%
                filter(
                  # no negative flow
                  q_lps > 0,
                  is.finite(q_lps),
                  # no negative concentration
                  con > 0,
                  is.finite(con))
              }

              q_log <- log10(paired_df$q_lps)
              c_log <- log10(paired_df$con)
              model_data <- tibble(c_log, q_log) %>%
                  filter(is.finite(c_log),
                         is.finite(q_log))%>%
                  na.omit()

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
              if(!is.nan(r_squared)) {
                if(r_squared > 0.3){
                    if(resid_acf > 0.2){
                        ideal_method <- 'composite'
                    }else{
                        ideal_method <- 'rating'
                    }
                }else{
                    if(con_acf > 0.20){
                        ideal_method <- 'pw'
                    }else{
                        ideal_method <- 'average'
                    }
                }
              } else {
                writeLines("\n\n ideal method error: r_squared value was NaN, ideal method set to NA\n\n")
                ideal_method <- NA
              }


              #### congeal fluxes ####

              if(period == 'monthly') {
                # reorder so all fluxes match pw months
                months <- sapply(strsplit(flux_annual_pw$date, "-"), `[`, 2)
                flux_monthly_comp <- flux_annual_comp[match(as.double(months), flux_annual_comp$month),]
                flux_monthly_average <- flux_annual_average[match(as.double(months), flux_annual_average$month),]

                target_year_out <- tibble(wy = as.character(target_year),
                                          month = rep(months, 5),
                                          val = c(flux_monthly_average$flux,
                                                  flux_annual_pw$flux,
                                                  flux_annual_beale$flux,
                                                  flux_annual_rating$flux,
                                                  ## flux_annual_wrtds,
                                                  flux_monthly_comp$flux
                                                  ),
                                    site_code = !!site_code,
                                    var = !!target_solute,
                                    method = c(rep('average', 12), rep('pw', 12), rep('beale', 12),
                                               rep('rating', 12), rep('composite', 12))) %>%
                    mutate(ms_recommended = ifelse(method == !!ideal_method, 1, 0))
              } else {
                target_year_out <- tibble(wy = as.character(target_year),
                                          val = c(flux_annual_average,
                                                  flux_annual_pw,
                                                  flux_annual_beale,
                                                  flux_annual_rating,
                                                  ## flux_annual_wrtds,
                                                  flux_annual_comp$flux[1]),
                                    site_code = !!site_code,
                                    var = !!target_solute,
                                    method = c('average', 'pw', 'beale', 'rating', 'composite')) %>%
                    mutate(ms_recommended = ifelse(method == !!ideal_method, 1, 0))
              }

              out_frame <- rbind(out_frame, target_year_out)

            } # good years

           } # if any rsfme methods

          } # if not simple
         } # end variables loop
      } # end sites loop

      if(any(method == 'simple')) {
        if(nrow(all_sites_flux) == 0) { return(NULL) }

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
      } # method return for siple or rsfme

  # TODO: allow monthly agg
  # TODO: make logic of simple vs RSFME clearer and articulate
  # TODO: make this work for *any* data in MacroSheds format
  # TODO: WRTDS inclusion
  # TODO: log file option
  # TODO: allow user select one or all methods?
  # TODO: make handling and make clear that RSFME methods are for q_type discharge only
}

ms_root = '../data/ms/'

 chemistry <- ms_load_product(macrosheds_root = ms_root,
                              prodname = 'stream_chemistry',
                              site_codes = c('w1'),
                              filter_vars = c('Na'))

 q <- ms_load_product(macrosheds_root = ms_root,
                      prodname = 'discharge',
                      site_codes = c('w1'))

 flux <- ms_calc_flux_rsfme(chemistry = chemistry,
                      q = q,
                      q_type = 'discharge',
                      method = c('beale', 'pw'))
