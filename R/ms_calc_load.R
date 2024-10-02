#' Calculate annual solute loads (i.e. cumulative fluxes)
#'
#' Determines solute loads from daily stream discharge and corresponding chemistry
#' data using any of five available methods. For daily stream/precip solute fluxes, see [ms_calc_flux()].
#'
#' @author Wes Slaughter, \email{wslaughter@@berkeley.edu}
#' @author Nick Gubbins, \email{gubbinsnick@@gmail.com}
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Spencer Rhea
#'
#' @param chemistry A `data.frame` of
#'    stream chemistry data in MacroSheds format (see details) and in units of mg/L.
#' @param q A `data.frame` of stream discharge (L/s) in MacroSheds format (see details).
#' @param method character vector. Any combination of: "pw", "rating",
#'    "composite", "beale", "average". Use "all" to run every method. See details.
# @param aggregation character. Either "monthly" or "annual". If "annual", each year
# is defined as the "water year" beginning on Oct 1 and ending on Sept 30.
#' @param verbose logical. FALSE for less frequent informational messaging.
#' @return
#' A `list` containing two tibbles, **load** and **diagnostics**. Tibble formats are as follows.
#'
#' **load**: a `tibble` of annual loads.
#' | column | definition |
#' | ------ | ---------- |
#' | site_code | short name for MacroSheds site. See MacroSheds format below. |
#' | var | Variable code. See MacroSheds format below. |
#' | water_year | Full year beginning on October 1 and ending on Sept 30 |
#' | load | Annual solute load for the watershed |
#' | method | The method used to compute annual solute load. See [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf) and [Gubbins et al. in review](https://eartharxiv.org/repository/view/6513/) |
#' | ms_recommended | The most appropriate load estimation method based on Fig 10 from [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf) and Fig 16 from [Gubbins et al. in review](https://eartharxiv.org/repository/view/6513/). 1 = recommended, 0 = not recommended. NA = insufficient data to generate meaningful load estimate by the corresponding method. |
#'
#' Output units are kg/ha/yr, where yr is water-year.
#'
#' **diagnostics**: a `tibble` of quantities that may be used to filter load estimates.
# 'Some of these are used to select `ms_recommended` according to
#' [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf) and
#' [Gubbins et al. in review](https://eartharxiv.org/repository/view/6513/).
#' | column | definition |
#' | ------ | ---------- |
#' | site_code | short name for MacroSheds site. See MacroSheds format below. |
#' | var | Variable code. See MacroSheds format below. |
#' | water_year | Full year beginning on October 1 and ending on Sept 30 |
#' | cq_rsquared | The coefficient of determination from the concentration-discharge relationship |
#' | cq_resid_acf | Autocorrelation coefficient at lag 1 on the residuals of the concentration-discharge relationship |
#' | c_acf | Autocorrelation coefficient at lag 1 for the concentration series |
#' | n_c_obs | Number of concentration observations |
#' | n_q_obs | Number of discharge observations |
#' | n_paired_cq_obs | Number of paired concentration and discharge observations |
#' | unique_c_quarters | Number of unique quarters (even divisions of the calendar year into four parts) represented in the concentration data |
#' | ws_area_ha | The watershed area in hectares |
#'
#' @details
#' MacroSheds format (only date, site_code, var, and val are required in inputs to this function):
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
#' Core time-series datasets generated by ms_load_product are in MacroSheds format.
#'
#' Load methods:
#'  + 'pw': period-weighted method, described in [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf). Here, C is linearly interpolated. This option uses package RiverLoad.
#'  + 'rating': AKA regression-model method, described in [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf). This option uses package RiverLoad.
#'  + 'composite': composite method, described in [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf). This option uses package RiverLoad.
#'  + 'beale': Beale ratio estimator, described in [Meals et al. 2013](https://www.epa.gov/sites/default/files/2016-05/documents/tech_notes_8_dec_2013_load.pdf). This option uses package RiverLoad.
#'  + 'average': mean Q over the aggregation period times mean C over the aggregation period
#'  Consult Figure 10 in [Aulenbach et al. 2016](https://www.esf.edu/srm/yanai/documents/Aulenbach_et_al-2016-Ecosphere.pdf)
#'  for guidance on method selection. See Figure 1 for some intuition.
#'
#' Output units are kg/ha/yr, where yr is water-year.
#'
#' References:
#'
#'  + Aulenbach, B. T., Burns, D. A., Shanley, J. B., Yanai, R. D., Bae, K., Wild, A. D., Yang, Y., & Yi, D. (2016). Approaches to stream solute load estimation for solutes with varying dynamics from five diverse small watersheds. Ecosphere, 7(6), e01298.
#'  + Meals, D. W., Richards, R. P., & Dressing, S. A. (2013). Pollutant load estimation for water quality monitoring projects. Tech Notes, 8, 1-21.
#'
#' @seealso [ms_calc_flux()]
#' @examples
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = 'hbef')

#' chemistry <- ms_load_product(macrosheds_root = ms_root,
#'                              prodname = 'stream_chemistry',
#'                              site_codes = c('w1', 'w3'),
#'                              filter_vars = c('NO3_N', 'Cl'))
#'
#' q <- ms_load_product(macrosheds_root = ms_root,
#'                      prodname = 'discharge',
#'                      site_codes = c('w1', 'w3'))
#'
#' flux <- ms_calc_load(chemistry = chemistry,
#'                      q = q)
#' @export
#'
ms_calc_load <- function(chemistry,
                         q,
                         method = 'all',
                         # aggregation = 'annual',
                         verbose = TRUE){

    aggregation <- 'annual'

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
    if(q_type != 'discharge'){
        stop('`q$var` must be "discharge".')
    }

    if(! inherits(q$date, 'Date')){
        q$date <- as.Date(q$date)
    }
    if(! inherits(chemistry$date, 'Date')){
        chemistry$date <- as.Date(chemistry$date)
    }

    requireNamespace('macrosheds', quietly = TRUE)

    # make sure method is accepted, and if method is 'all' set method to all rsfme methods
    # check that method, if non-null, is in accepted list
    rsfme_accepted  <- c('average', 'pw', 'composite', 'beale', 'rating', 'all')
    rsfme_methods  <- c('average', 'pw', 'composite', 'beale', 'rating')

    if('all' %in% method){
        method <- rsfme_methods
    }

    if(! all(method %in% rsfme_accepted)){
        stop(glue::glue('Unrecognized flux method: {setdiff(method, rsfme_accepted)}'))
    } else {
        if(verbose){
            message(glue::glue('calculating flux using method(s): {m}',
                               m = paste(method, collapse = ', ')))
        }
    }

    got_riverload <- requireNamespace('RiverLoad', quietly = TRUE)
    got_imputets <- requireNamespace('imputeTS', quietly = TRUE)

    if(! got_imputets && ! got_riverload){
        stop('The "RiverLoad" and "imputeTS" packages are required to run ms_calc_load. ',
             'Install with:\n\tremotes::install_github("cran/RiverLoad")',
             '\n\tinstall.packages("imputeTS")')
    } else if(! got_imputets){
        stop('The "imputeTS" package is required to run ms_calc_load. ',
             'Install it with:\n\tinstall.packages("imputeTS")')
    } else if(! got_riverload){
        stop('The "RiverLoad" package is required to run ms_calc_load. ',
             'Install it with:\n\tremotes::install_github("cran/RiverLoad")')
    }

    if(is.null(aggregation) || length(aggregation) != 1 || ! aggregation %in% c('annual', 'monthly')){
        stop('`aggregation` must be either "annual" or "monthly"')
    }

    var_info <- macrosheds::ms_load_variables() %>%
        dplyr::select(variable_code, flux_convertible) %>%
        distinct()
    site_info <- macrosheds::ms_load_sites()

    # verify that both files have the same sites
    sites_chem <- unique(chemistry$site_code)
    sites <- sites_chem
    sites_q <- unique(q$site_code)

    if(! setequal(sites_chem, sites_q)){
        stop('`chemistry` and `q` must have the same sites.')
    }

    ## add errors if they don't exist
    #if('val_err' %in% colnames(chemistry)){
    #
    #    errors::errors(chemistry$val) <- chemistry$val_err
    #    chemistry <- dplyr::select(chemistry, -val_err)
    #
    #} else if(! inherits(chemistry$val, 'errors')){
    #    errors::errors(chemistry$val) <- 0
    #}
    #
    #if('val_err' %in% colnames(q)){
    #
    #    errors::errors(q$val) <- q$val_err
    #    q <- dplyr::select(q, -val_err)
    #
    #} else if(! inherits(q$val, 'errors')){
    #    errors::errors(q$val) <- 0
    #}
    chemistry <- dplyr::select(chemistry, -any_of('val_err'))
    q <- dplyr::select(q, -any_of('val_err'))

    load_out <- diag_out <- tibble()
    for(s in seq_along(sites)){

        if(verbose) message('Working on site ', s, ' of ', length(sites))

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
        if(is.na(area)){
            warning('Watershed area for site "', site_code, '" is unknown')
        }
        # area <- errors::set_errors(this_site_info$ws_area_ha, 0)

        vars_ <- unique(site_chem$var)

        load_site <- diag_site <- tibble()
        for(i in seq_along(vars_)){

            chem_var <- site_chem %>%
                filter(var == !!vars_[i]) %>%
                arrange(date)

            fluxable <- var_info %>%
                filter(variable_code == !!ms_drop_var_prefix_(vars_[i])) %>%
                pull(flux_convertible)

            if(any(fluxable == 0)){
                warning(glue::glue('Flux methods not yet defined for {vars_[i]}; skipping'))
                next
            }

            load_out_ <- calc_load(
                chem = chem_var,
                q = site_q,
                site_code = site_code,
                area = area,
                method = method,
                aggregation = aggregation,
                verbose = verbose
            )

            diag_site <- bind_rows(load_out_$diag, diag_site)
            load_site <- bind_rows(load_out_$load, load_site)
        }

        diag_out <- bind_rows(diag_site, diag_out)
        load_out <- bind_rows(load_site, load_out)
    }

    if(nrow(load_out)){

        load_out <- load_out %>%
            relocate(site_code, var, wy) %>%
            rename(water_year = wy, load = val) %>%
            arrange(site_code, var, water_year) %>%
            filter(! is.na(load))

        diag_out <- arrange(diag_out, site_code, var, water_year)

    } else {

        load_out <- tibble(site_code = character(),
                           var = character(),
                           water_year = numeric(),
                           load = numeric(),
                           method = character(),
                           ms_recommended = logical())

        diag_out <- tibble(site_code = character(),
                           var = character(),
                           water_year = numeric(),
                           cq_rsquared = numeric(),
                           cq_resid_acf = numeric(),
                           c_acf = numeric(),
                           n_c_obs = numeric(),
                           n_q_obs = numeric(),
                           n_paired_cq_obs = numeric(),
                           ws_area_ha = numeric())
    }

    # load_out <- mutate(load_out, val = if_else(val < 0, 0, val))
    if(any(load_out$load < 0) || any(is.infinite(load_out$load))){
        warning('Negative/infinite load values detected.')
    }

    return(list(load = load_out,
                diagnostics = diag_out))
}
