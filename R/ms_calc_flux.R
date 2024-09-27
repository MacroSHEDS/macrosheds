#' Calculate daily solute fluxes
#'
#' Determines solute fluxes from interpolated daily Q (discharge
#' or precipitation) and corresponding chemistry data.
#' For cumulative solute loads computed by various methods, see [ms_calc_load()].
#'
#' @author Spencer Rhea
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param chemistry A \code{data.frame} of precipitation or
#'    stream chemistry data in MacroSheds format (see details) and in units of mg/L.
#' @param q A \code{data.frame} in MacroSheds format (see details) containing
#' stream discharge in L/s or precipitation in mm.
#' @return returns a \code{tibble} in MacroSheds format (see details) of stream or
#' precipitation chemical flux for every dayduring which discharge/precipitation
#' and chemistry are reported. Output units are kg/ha/d.
#' @details
#' #' MacroSheds format (only date, site_code, var, and val are required in inputs to this function):
#' | column        | definition        |
#' | ------------- | ----------------- |
#' | date          | Date in YYYY-mm-dd |
#' | site_code     | A unique identifier for each MacroSheds site, identical to primary source site code where possible. See [ms_load_sites()]. |
#' | grab_sample   | Boolean integer indicating whether the observation was obtained via grab sample or installed sensor. 1 = TRUE (grab sample), 0 = FALSE (installed sensor). |
#' | var           | Variable code. See [ms_load_variables()]. |
#' | val           | Data value. See [ms_load_variables()] for units. |
#' | ms_status     | Boolean integer. 0 = clean value. 1 = questionable value. See "Technical Validation" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#' | ms_interp     | Boolean integer. 0 = measured or imputed by primary source. 1 = interpolated by MacroSheds. See "Temporal Imputation and Aggregation" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#' | val_err       | The combined standard uncertainty associated with the corresponding data point, if estimable. See "Detection Limits and Propagation of Uncertainty" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#'
#' This is the format returned by ms_load_product for core time-series data.
#'
#' Our Solute fluxes are calculated by multiplying solute concentration by daily water volume.
#' Stream and precipitation flux are computed differently. For stream flux (stream chemistry * discharge),
#' flux is: kg/ha/d = mg/L \* L/s \* 86400 / 1e6 / ws_area.
#' Precipitation flux (precip chemistry \* precip depth over the watershed)
#' is calculated as: kg/ha/d = mg/L \* mm/d / 100.
#' You can convert between kg/ha/d and kg/d using [ms_scale_flux_by_area()] and
#' [ms_undo_scale_flux_by_area()].
#'
#' @seealso [ms_calc_load()], [ms_scale_flux_by_area()], [ms_undo_scale_flux_by_area()]
#' @export
#' @examples
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = 'hbef')
#'
#' chemistry <- ms_load_product(macrosheds_root = ms_root,
#'                              prodname = 'stream_chemistry',
#'                              site_codes = c('w1', 'w3'),
#'                              filter_vars = c('NO3_N', 'Cl'))
#'
#' q <- ms_load_product(macrosheds_root = ms_root,
#'                      prodname = 'discharge',
#'                      site_codes = c('w1', 'w3'))
#'
#' flux <- ms_calc_flux(chemistry = chemistry,
#'                      q = q)

ms_calc_flux <- function(chemistry, q){

    library('dplyr', quietly = TRUE)

    #checks
    if(! all(c('site_code', 'val', 'var', 'date') %in% names(chemistry))){
        stop('The argument to `chemistry` must be in MacroSheds format (required columns: date, site_code, var, val).')
    }
    if(! all(c('site_code', 'val', 'var', 'date') %in% names(q))){
        stop('The argument to `q` must be in MacroSheds format (required columns: date, site_code, var, val).')
    }

    q_type <- unique(q$var)
    if(length(q_type) > 1){
        stop('q$var` may not contain more than 1 unique value')
    }
    if(! q_type %in% c('precipitation', 'discharge')){
        stop('`q$var` must be entirely "discharge" or "precipitation".')
    }

    if(! inherits(q$date, 'Date')){
        q$date <- as.Date(q$date)
    }
    if(! inherits(chemistry$date, 'Date')){
        chemistry$date <- as.Date(chemistry$date)
    }

    requireNamespace('macrosheds', quietly = TRUE)

    var_info <- macrosheds::ms_load_variables()
    site_info <- macrosheds::ms_load_sites()

    # verify that both files have the same sites
    sites_chem <- unique(chemistry$site_code)
    sites <- sites_chem
    sites_q <- unique(q$site_code)

    if(! setequal(sites_chem, sites_q)){
        stop('`chemistry` and `q` must have the same sites.')
    }

    # # Check the intervals are the same in both chemistry and q
    # qdiff <- diff(as.numeric(q$date))
    # qdiff <- qdiff[qdiff != 0]
    # q_interval <- Mode(qdiff)
    #
    # interval <- case_when(q_interval == 86400 ~ 'daily',
    #                       q_interval == 3600 ~ 'hourly',
    #                       q_interval == 1800 ~ '30 minute',
    #                       q_interval == 960 ~ '15 minute',
    #                       q_interval == 600 ~ '10 minute',
    #                       q_interval == 300 ~ '5 minute',
    #                       q_interval == 60 ~ '1 minute')
    #
    # # q_interval <- errors::as.errors(q_interval)
    #
    # flow_is_highres <- Mode(diff(as.numeric(q$datetime))) <= 15 * 60
    # if(is.na(flow_is_highres)) { flow_is_highres <- FALSE }
    #
    # # if(is.na(interval)){
    # #     stop(paste0('interval of samples must be one',
    # #                 ' of: daily, hourly, 30 minute, 15 minute, 10 minute, 5 minute, or 1 minute.',
    # #                 ' See macrosheds::ms_synchronize_timestep() to standardize your intervals.'))
    # # }
    # if(interval != 'daily') stop('sample interval must be daily')

    # # add errors if they don't exist
    # if('val_err' %in% names(chemistry)){
    #     errors::errors(chemistry$val) <- chemistry$val_err
    #
    #     chemistry <- chemistry %>%
    #         dplyr::select(-val_err)
    #
    # } else if(all(errors::errors(chemistry$val) == 0)){
    #     errors::errors(chemistry$val) <- 0
    # }
    #
    # if('val_err' %in% names(q)){
    #     errors::errors(q$val) <- q$val_err
    #
    #     q <- q %>%
    #         dplyr::select(-val_err)
    #
    # } else if(all(errors::errors(q$val) == 0)){
    #     errors::errors(q$val) <- 0
    # }
    chemistry <- select(chemistry, -any_of('val_err'))
    q <- select(q, -any_of('val_err'))

    chemistry <- chemistry %>%
        left_join(select(var_info, variable_code, flux_convertible),
                  by = c(var ='variable_code'))

    nonconvertible <- chemistry %>%
        filter(is.na(flux_convertible) | flux_convertible != 1) %>%
        pull(var) %>%
        unique()

    if(length(nonconvertible)){
        warning(glue::glue('Flux methods not yet defined for {vv}. These variables will be dropped.',
                           vv = paste(nonconvertible, collapse = ', ')))
        chemistry <- filter(chemistry, ! is.na(flux_convertible & flux_convertible == 1))
    }

    chemistry <- select(chemistry, -flux_convertible)

    # calc flux
    all_sites_flux <- tibble()
    for(s in seq_along(sites)){

        site <- sites[s]

        site_chem <- filter(chemistry, site_code == !!site)
        site_q <- filter(q, site_code == !!site)

        daterange <- as.Date(range(site_chem$date))
        site_q <- site_q %>%
            filter(
                date >= !!daterange[1],
                date <= !!daterange[2])

        if(! nrow(site_q)){
            warning(glue::glue('Site {site} has no Q-C overlap; skipping.'))
            next
        }

        chem_split <- site_chem %>%
            group_by(var) %>%
            arrange(date) %>%
            dplyr::group_split() %>%
            as.list()

        vars_ <- unique(site_chem$var)

        # Loop though all variables
        for(i in seq_along(chem_split)){

            chem_chunk <- chem_split[[i]]

            # chem_is_highres <- Mode(diff(as.numeric(chem_chunk$datetime))) <= 15 * 60
            # if(is.na(chem_is_highres)){ chem_is_highres <- FALSE}
            #
            # #if both chem and flow data are low resolution (grab samples),
            # #   let approxjoin_datetime match up samples with a 12-hour gap. otherwise the
            # #   gap should be 7.5 mins so that there isn't enormous duplication of
            # #   timestamps where multiple high-res values can be snapped to the
            # #   same low-res value
            # if(! chem_is_highres && ! flow_is_highres) {
            #     join_distance <- c('12:00:00')#, '%H:%M:%S')
            # } else {
            #     join_distance <- c('7:30')#, '%M:%S')
            # }
            #
            # chem_split[[i]] <- approxjoin_datetime(x = chem_chunk,
            #                                        y = site_q,
            #                                        rollmax = join_distance,
            #                                        keep_datetimes_from = 'x')

            chem_split[[i]] <- calc_simple_flux(chem_chunk, site_q)
        }

        flux <- chem_split %>%
            purrr::reduce(bind_rows) %>%
            arrange(site_code, var, date)

        all_sites_flux <- rbind(all_sites_flux, flux)
    }

    # all_sites_flux$val_err <- errors::errors(all_sites_flux$val)
    # all_sites_flux$val <- errors::drop_errors(all_sites_flux$val)

    return(all_sites_flux)
}
