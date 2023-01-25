#' Calculates chemical fluxes
#'
#' Calculates stream discharge and precipitation fluxes of chemicals from Q (discharge
#' or precipitation) and chemistry data.
#'
#' @author Spencer Rhea, spencerrhea41@@gmail.com
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param chemistry \code{data.frame}. A \code{data.frame} of precipitation or
#'    stream chemistry data in MacroSheds format and in units of mg/L.
#' @param q \code{data.frame}. A \code{data.frame} of precipitation or stream
#'    discharge in MacroSheds format and in units of mm or L/s, respectively.
#' @param q_type character. Either 'precipitation' or 'discharge'.
#' @param verbose logical. Default TRUE; prints more information to console.
#' @return returns a \code{tibble} of stream or precipitation chemical flux for every timestep
#'    where discharge/precipitation and chemistry are reported. Output units are kg/ha/timestep.
#' @details
#' Chemical Volume Weighted Concentration (VWC) is 'traditionally' calculated as
#' VWC = sum(concentrations * volumes) / sum(volumes) over a given period. Because we have discharge
#' (a rate), rather than volume, we could scale it to the sample interval and then use the equation
#' above. For simplicity, I use this instead, which works out the same:
#' VWC = mean(concentrations * discharges) / mean(discharges)
#' Note also that we're starting from flux here, for computational efficiency, so the "concentrations * volumes"
#' part is already computed, and it's a rate too. That means the modified equation is:
#' VWC = mean(fluxes) / mean(discharges). There are two minor introductions of
#' error. The first is that we don't omit discharge values from the denominator that lack a corresponding
#' flux value (this is expensive, and not doing it should rarely skew the results). The second is that a "month"
#' is considered 30 days here, which only affects coverage filtering.
#' @seealso [ms_calc_flux], [ms_synchronize_timestep()], [ms_conversions()], [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @examples
#' #' ### Load some MacroSheds data:
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = 'hbef')
#'
#' chemistry <- ms_load_product(macrosheds_root = ms_root,
#'                              prodname = 'stream_chemistry',
#'                              site_codes = c('w1', 'w3', 'w6'),
#'                              filter_vars = c('NO3_N', 'Cl', 'Na'))
#'
#' q <- ms_load_product(macrosheds_root = ms_root,
#'                      prodname = 'discharge',
#'                      site_codes = c('w1', 'w3', 'w6'))
#'
#' flux <- ms_calc_vwc(chemistry = chemistry,
#'                      q = q,
#'                      q_type = 'discharge')

ms_calc_vwc <- function(chemistry, q, q_type, agg = "yearly", verbose = TRUE) {

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

    # calc VWC
    sites <- unique(chemistry$site_code)

    all_sites_vwc <- tibble()
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
          if(agg == "monthly") {
            agg_step <- c("year", "month")
            threshold <- 3
          } else if(agg == "yearly") {
            agg_step <- c("year")
            threshold <- 12
          } else {
            stop("user input aggregation type not recognized. accepted VWC aggregation time frames are 'monthly' and yearly'")
          }

          chem_split[[i]] <- chem_split[[i]] %>%
            filter_at(vars(val_x, val_y),
                      all_vars(!is.na(.))) %>%
            mutate(month = lubridate::month(datetime),
                   year = lubridate::year(datetime)) %>%
            group_by_at(agg_step) %>%
            summarize(site_code = site_code_x,
                      var = var_x,
                      val = mean(val_x * val_y)/mean(val_y),
                      n = n(),
                      ms_status = numeric_any_v(ms_status_x, ms_status_y),
                      ms_interp = numeric_any_v(ms_interp_x, ms_interp_y)) %>%
            select(-starts_with(c('site_code_', 'var_', 'val_',
                                  'ms_status_', 'ms_interp_'))) %>%
            distinct() %>%
            arrange_at(agg_step)

          sub_pars <- sum(chem_split[[i]]$n < threshold)

          if(sub_pars > 0) {
            warning(paste(sub_pars, agg, "VWCs calculated with less than", threshold, "contributing samples"))
          }
        }

        vwc <- chem_split %>%
            purrr::reduce(bind_rows) %>%
            arrange_at(c("site_code", "var", agg_step))

        all_sites_vwc <- rbind(all_sites_vwc, vwc)

    }

    if(nrow(all_sites_vwc) == 0) { return(NULL) }

    all_sites_vwc$val_err <- errors::errors(all_sites_vwc$val)
    all_sites_vwc$val <- errors::drop_errors(all_sites_vwc$val)

    return(all_sites_vwc)
}
