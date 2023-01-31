#' Calculates chemical fluxes
#'
#' Calculates solute fluxes from Q (discharge
#' or precipitation) and chemistry data.
#'
#' @author Wes Slaughter, \email{wslaughter@berkeley.edu}
#' @author Nick Gubbins, \email{gubbinsnick@gmail.com}
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
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
#' flux <- ms_calc_flux(chemistry = chemistry,
#'                      q = q,
#'                      q_type = 'discharge')

ms_calc_flux_rsfme <- function(chemistry, q, q_type, verbose = TRUE,
                         method = 'simple', aggregation = 'simple') {

    library("dplyr", quietly = TRUE)

    #### Checks
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(chemistry))){
        stop('The argument to chemistry must contain precipitation chemistry or stream chemistry data in MacroSheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum).')
    }
    if(! all(c('site_code', 'val', 'var', 'datetime', 'ms_interp', 'ms_status') %in% names(q))){
        stop('The argument to q must contain precipitation or stream discharge data in MacroSheds format (column names of site_code, val, var, datetime, ms_interp, ms_status at minimum).')
    }
    if(! grepl('(precipitation|discharge)', q_type)){
        stop('q_type must be "discharge" or "precipitation"')
    }
    if(! 'POSIXct' %in% class(q$datetime)){
        q$datetime <- as.POSIXct(q$datetime)
    }
    if(! 'POSIXct' %in% class(chemistry$datetime)){
        chemistry$datetime <- as.POSIXct(chemistry$datetime)
    }

    requireNamespace('macrosheds', quietly = TRUE)

    # check that method, if non-null, is in accepted list
    rsfme_accepted <- c('average', 'pw', 'composite', 'wrtds', 'beale', 'simple')
    if(!method %in% rsfme_accepted) {
      stop(glue('method supplied is not in accepted list, must be one of the following:\n {list}',
                list = rsfme_accepted))
    } else {
      writeLines(glue('calculating flux using method: {method}', method = method))
    }

    # make sure agg option is annual or monthly if calculating any non-null method
    # and otherwise timestep is data-res and using simple QC
    rsfme_aggs <- c('annual', 'monthly', 'simple')
    if(!aggregation %in% rsfme_aggs) {
      stop(glue::glue('time aggregation is not in accepted list, must be one of the following:\n {list}',
                list = rsfme_aggs))
    } else if(aggregation == 'simple') {
      writeLines(glue::glue('calculating flux at highest possible resolution timestep of data supplied, using simple Q*C methods', aggregation = aggregation))
    } else {
      writeLines(glue::glue('calculating flux over: {aggregation}', aggregation = aggregation))
    }

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
        print(paste0('q dataset has a ', interval, ' interval'))
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
            # df of just one solute chem at one site, over all time
            chem_chunk <- chem_split[[i]]

            # target solute
            target_solute <- unique(chem_chunk %>% pull(var))

            writeLines(glue::glue('________\n\nformula: {method}\nsolute: {solute}\n________', method = method, solute = target_solute ))

            # 'good year' checks for RSFME calcs
            if(method != 'simple') {

              # df to populate with annual flux values by method
              out_frame <- tibble(wy = as.character(),
                    site_code = as.character(),
                    val = as.numeric(),
                    var = as.character(),
                    method = as.character())
                    ## ms_reccomended = as.integer(),
                    ## ms_interp_ratio = as.numeric(),
                    ## ms_status_ratio = as.numeric(),
                    ## ms_missing_ratio = as.numeric())


              # find acceptable years
              q_check <- raw_data_q %>%
                  mutate(date = date(datetime)) %>%
                  # NOTE: should we filter out NAs?
                  filter(ms_interp == 0, !is.na(val)) %>%
                  distinct(., date, .keep_all = TRUE) %>%
                  mutate(wtr_yr = wtr_yr(datetime, start_month = 10)) %>%
                  group_by(wtr_yr) %>%
                  summarise(n = n()) %>%
                  filter(n >= 311)

              conc_check <- raw_data_con %>%
                  mutate(date = date(datetime)) %>%
                  # NOTE: should we filter out NAs?
                  filter(!is.na(val)) %>%
                  distinct(., date, .keep_all = TRUE) %>%
                  mutate(wtr_yr = wtr_yr(date, start_month = 10),
                         quart = quarter(date)) %>%
                  group_by(wtr_yr) %>%
                  summarise(count = n_distinct(quart),
                            n = n()) %>%
                  filter(n >= 4,
                         count > 3)


              q_good_years <- q_check$wtr_yr
              conc_good_years <- conc_check$wtr_yr

              # 'good years' where Q and Chem data both meet min requirements
              good_years <- q_good_years[q_good_years %in% conc_good_years]
              n_yrs <- length(good_years)

              # NOTE: adding handling if concentration data fails conc check
              if(nrow(conc_check) < 1) {
                writeLines(glue::glue("{site} concentration data insufficient sample size and frequency to warrant flux estimation",
                                "\n   no water years in {site} dataset with minimum standards met", site = site_code))
                next
              } else if(nrow(q_check) < 1) {
                writeLines(glue::glue("{site} discharge data insufficient sample size and frequency to warrant flux estimation",
                                "\n   no water years in {site} dataset with minimum standards met", site = site_code))
                next
              } else if(length(good_years) == 0) {
                writeLines(glue::glue("no water years where q data and concentration data both meet minimum standards",
                      "skipping site: {site}", site = site_code))
                next
              }

              #join data and cut to good years
              daily_data_con <- raw_data_con %>%
                  mutate(date = date(datetime)) %>%
                  group_by(date) %>%
                  summarize(val = mean_or_x(val)) %>%
                  mutate(site_code = !!site_code, var = 'con') %>%
                  select(site_code, datetime = date, var, val)

              daily_data_q <- raw_data_q %>%
                  mutate(date = date(datetime)) %>%
                  group_by(date) %>%
                  summarize(val = mean_or_x(val)) %>%
                  mutate(site_code = !!site_code, var = 'q_lps') %>%
                  select(site_code, datetime = date, var, val)

              q_df <- daily_data_q %>%
                pivot_wider(names_from = var,
                            values_from = val)

              raw_data_full <- rbind(daily_data_con, daily_data_q) %>%
                  pivot_wider(names_from = var, values_from = val, id_cols = c(site_code, datetime)) %>%
                  mutate(wy = wtr_yr(datetime, start_month = 10)) %>%
                  filter(wy %in% good_years)

               con_full <- raw_data_full %>%
                   mutate(wy = as.numeric(as.character(wy))) %>%
                     select(site_code, datetime, con, wy) %>%
                     ## filter(wy < 1975) %>%
                 na.omit()



              if(tolower(method) == 'wrtds') {

                stop('WRTDS currently not available in ms_calc_flux')

                # check for if there are > 100 observations (rows)
                if(nrow(con_full) < 100) {
                  flux_annual_wrtds <- NA
                  return(flux_annual_wrtds)
                }
                # check if median value is greater than .5
                if(median(con_full$con) < 0.5) {
                  flux_annual_wrtds <- NA
                  return(flux_annual_wrtds)
                }

                #### calculate WRTDS ######
                tryCatch(
                  expr = {
                    flux_annual_wrtds <- calculate_wrtds(
                      chem_df = con_full,
                      q_df = q_df,
                      ws_size = area,
                      lat = lat,
                      long = long,
                      datecol = 'datetime',
                      agg = 'annual',
                      minNumObs = 100,
                      minNumUncen = 50
                     )

                    wrtds_out <- flux_annual_wrtds %>%
                         filter(wy %in% good_years) %>%
                         rename(val = flux) %>%
                         mutate(site_code = site_code,
                              var = solutes[j],
                              method = 'wrtds',
                              ms_recommended = 0)

                        return(wrtds_out)
                  },
                  error = function(e) {
                    writeLines(paste('\nWRTDS run failed for \n     site', site_code,
                                     '\n     variable', target_solute, '\n WRTDS TRYING AGAIN'))
                    tryCatch(
                      expr = {
                        flux_annual_wrtds <- calculate_wrtds(
                               chem_df = con_full,
                               q_df = q_df,
                               ws_size = area,
                               lat = lat,
                               long = long,
                               datecol = 'datetime',
                               agg = 'annual',
                               minNumObs = 100,
                               minNumUncen = 50
                        )

                       wrtds_out <- flux_annual_wrtds %>%
                         filter(wy %in% good_years) %>%
                         rename(val = flux) %>%
                         mutate(site_code = site_code,
                              var = solutes[j],
                              method = 'wrtds',
                              ms_recommended = 0)

                        return(wrtds_out)
                      },
                      error = function(e) {
                        print("WRTDS failed, setting to NA")
                        flux_annual_wrtds <- NA
                      }
                    )
                  }
                ) # end wrtds

              }


              # if not wrtdsk
              for(k in 1:length(good_years)){

               writeLines(paste("site:", site_code,
                              'year:', good_years[k]))

               target_year <- as.numeric(as.character(good_years[k]))

               # calculate flag ratios to carry forward
               flag_df <- carry_flags(raw_q_df = raw_data_q,
                                      raw_con_df = raw_data_con_in,
                                      target_year = target_year,
                                      target_solute = target_solute,
                                      period = 'annual')

               raw_data_target_year <- raw_data_full %>%
                   mutate(wy = as.numeric(as.character(wy))) %>%
                   filter(wy == target_year)

               q_target_year <- raw_data_target_year %>%
                   select(site_code, datetime, q_lps, wy)%>%
                   na.omit()

               con_target_year <- raw_data_target_year %>%
                   select(site_code, datetime, con, wy) %>%
                   na.omit()

               ### calculate annual flux ######
               chem_df_errors <- con_target_year
               q_df_errors <- q_target_year

               ### save and then remove errors attribute for calcs
               chem_df <- errors::drop_errors(chem_df_errors)
               q_df <- errors::drop_errors(q_df_errors)

              if(method == 'average') {
               #### calculate average ####
                flux_annual <- raw_data_target_year %>%
                   group_by(wy) %>%
                   summarize(q_lps = mean(q_lps, na.rm = TRUE),
                             con = mean(con, na.rm = TRUE)) %>%
                   # multiply by seconds in a year, and divide my mg to kg conversion (1M)
                   mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
                 pull(flux)
              } else if(method == 'pw') {
                 #### calculate period weighted #####
                 flux_annual <- calculate_pw(chem_df, q_df, datecol = 'datetime')
              } else if (method =='beale') {
                 #### calculate beale ######
                 flux_annual <- calculate_beale(chem_df, q_df, datecol = 'datetime')

              } else if (method == 'rating') {
                 #### calculate rating #####
                 flux_annual <- calculate_rating(chem_df, q_df, datecol = 'datetime')
              } else if (method == 'composite') {
                  #### calculate composite ######
                  rating_filled_df <- generate_residual_corrected_con(chem_df = chem_df,
                                                                      q_df = q_df,
                                                                      datecol = 'datetime',
                                                                      sitecol = 'site_code')
                  # calculate annual flux from composite
                  flux_annual_comp <- calculate_composite_from_rating_filled_df(rating_filled_df)
                  flux_annual <- flux_annual_comp$flux[1]
              }

                 #### congeal fluxes ####
                 target_year_out <- tibble(wy = as.character(target_year),
                                           val = flux_annual,
                                           site_code = !!site_code,
                                           var = !!target_solute,
                              method = !!method)
                 out_frame <- bind_rows(out_frame, target_year_out)
              } # end year loop

            } else {

              # back to simple flux
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
                                    select(-starts_with(c('site_code_', 'var_', 'val_',
                                                          'ms_status_', 'ms_interp_'))) %>%
                                    filter(! is.na(val)) %>% #should be redundant
                                    arrange(datetime)
                            }

                        flux <- chem_split %>%
                            purrr::reduce(bind_rows) %>%
                            arrange(site_code, var, datetime)

                    all_sites_flux <- rbind(all_sites_flux, flux)


                    if(nrow(all_sites_flux) == 0) { return(NULL) }

                    all_sites_flux$val_err <- errors::errors(all_sites_flux$val)
                    all_sites_flux$val <- errors::drop_errors(all_sites_flux$val)

                    return(all_sites_flux)
                }
            }
    }

    return(out_frame)
}
