#' Aggregate or interpolate MacroSheds data to specified time intervals
#'
#' Set the sample interval of a \code{data.frame} in MacroSheds
#' format (see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262))
#' by interpolation (for upsampling) or aggregation (for downsampling).
#'
#' @keywords internal
#' @author Spencer Rhea 
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param d \code{data.frame}. A \code{data.frame} in MacroSheds format (see [MacroSheds EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262)
#'    which can be generated by [ms_load_product()].
#' @param desired_interval character. Input that can be parsed by the "by"
#'     parameter of base::seq.POSIXt, e.g. "5 mins", "1 day", "1 month".
#' @param round_method character. If "floor" (the default), dates or datetimes will be shifted
#'     backward to the nearest \code{desired_interval} boundary by [lubridate::floor_date()]. For example,
#'     with \code{desired_interval} set to "1 day", 2020-01-01 23:59:00 would be adjusted to 2020-01-01 00:00:00.
#'     The other option is to "round" dates or datetimes to the nearest interval boundary via [lubridate::round_date()].
#' @param interpolate_method character. One of:
#'     \itemize{
#'       \item "linear": the default. Linear interpolation.
#'       \item "nocb": next observation carried backward. Useful for interpolating precipitation chemistry reported as concentration.
#'       \item "nocb_mean": the next observation, divided by the number of preceding observations that are missing, carried backward. Useful for e.g. precipitation measured with a collector after x days.
#'       \item "zero": replace missing values with 0. Useful for e.g. precipitation series where nothing is reported during periods of no rain/snow.
#'     }
#' @param interpolate_limit numeric. The maximum number of consecutive points to interpolate.
#' @param summary_fun optional character. One of "mean" or "sum". The function that will be used to 
#'     summarize data if desired_interval is larger than the input data in d.
#' @param quiet logical. Should warnings be printed to console.
#' @return returns a \code{tibble} with data in the new desired_interval with a 
#'     ms_interp column indicating if a value was interpolated.
#' @examples
#' ### Load some MacroSheds data:
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root,
#'                       domains = 'hbef')
#' d <- ms_load_product(macrosheds_root = ms_root, 
#'                      prodname = 'stream_chemistry',
#'                      domains = 'hbef',
#'                      filter_vars = c('NO3_N', 'Na', 'Mg', 'SO4_S'))
#'                      
#' ### interpolate to 15-minute intervals, but don't fill gaps larger than 24 hours
#' ms_synchronize_timestep(d, '15 min', 96)
#' 
#' ### aggregate samples to weekly interval by mean, and fill gaps of < 3 weeks
#' ms_synchronize_timestep(d, '1 week', 3, summary_fun = 'mean')

ms_synchronize_timestep <- function(d,
                                    desired_interval,
                                    round_method = 'floor',
                                    interpolate_method = 'linear',
                                    interpolate_limit,
                                    summary_fun,
                                    quiet = FALSE){

    library("dplyr", quietly = TRUE)

    check_suggested_pkgs(c('data.table'))

    # Checks 
    if(nrow(d) < 2 || sum(! is.na(d$val)) < 2){
        stop('not enough data to synchronize')
    }
    
    if(! missing(summary_fun) && ! summary_fun %in% c('sum', 'mean')) {
        stop('summary_fun must be either "sum" or "mean"')
    }

    if(! interpolate_method %in% c('linear', 'nocb', 'nocb_mean', 'zero')){
        stop('invalid argument to interpolate_method')
    }
    
    if(! round_method %in% c('round', 'floor')){
        stop('round_method must be "floor" or "round"')
    }

    requireNamespace('macrosheds', quietly = TRUE)
    
    #split dataset by site and variable.
    d_split <- d %>%
        group_by(site_code, var) %>%
        arrange(datetime) %>%
        dplyr::group_split() %>%
        as.list()

    for(i in 1:length(d_split)){

        sitevar_chunk <- d_split[[i]]
        
        n_dupes <- sum(duplicated(sitevar_chunk$datetime) |
                           duplicated(sitevar_chunk$datetime,
                                      fromLast = TRUE))
        
        # average/sum values for duplicate timestamps
        if(n_dupes > 0){
            
            msg <- glue::glue('{n} duplicate datetimes found for site: {s}, var: {v}',
                        n = n_dupes,
                        s = sitevar_chunk$site_code[1],
                        v = sitevar_chunk$var[1])
            
            if(!quiet){
                print(msg)
            }
            
            sitevar_chunk_dt <- sitevar_chunk %>%
                data.table::as.data.table()
            
            #take the mean value for any duplicate timestamps
            sitevar_chunk <- sitevar_chunk_dt[, .(
                site_code = data.table::first(site_code),
                var = data.table::first(var),
                #data.table doesn't work with the errors package, but we're
                #determining the uncertainty of the mean by the same method here:
                #    max(SDM, mean(uncert)),
                #    where SDM is the Standard Deviation of the Mean.
                #The one difference is that we remove NAs when computing
                #the standard deviation. Rationale: 1. This will only result in
                #"incorrect" error values if there's an NA error coupled with a
                #non-NA value (if the value is NA, the error must be too).
                #I don't think this happens very often, if ever.
                #2. an error of NA just looks like 0 error, which is more misleading
                #than even a wild nonzero error.
                val_err = max(sd_or_0(val, na.rm = TRUE) / sqrt(.N),
                              mean(val_err, na.rm = TRUE)),
                val = mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status)
            ), keyby = datetime] %>%
                as_tibble()
        }

        #round each site-variable tibble's datetime column to the desired interval.
        if(round_method == 'floor'){
            sitevar_chunk <- mutate(sitevar_chunk,
                                    datetime = lubridate::floor_date(
                                        x = datetime,
                                        unit = desired_interval))
        } else {
            sitevar_chunk <- mutate(sitevar_chunk,
                                    datetime = lubridate::round_date(
                                        x = datetime,
                                        unit = desired_interval))
        }
        
        #split chunk into subchunks. one has duplicate datetimes to summarize,
        #   and the other doesn't. both will be interpolated in a bit.
        to_summarize_bool <- duplicated(sitevar_chunk$datetime) |
            duplicated(sitevar_chunk$datetime,
                       fromLast = TRUE)
        
        summary_and_interp_chunk <- sitevar_chunk[to_summarize_bool, ]
        interp_only_chunk <- sitevar_chunk[! to_summarize_bool, ]
        
        if(nrow(summary_and_interp_chunk)){

            if(missing(summary_fun)){
                stop('summary_fun of "mean" or "sum" is required when aggregating data')
            }
            use_sum <- ifelse(summary_fun == 'sum', TRUE, FALSE)
            
            summary_and_interp_chunk_dt <- summary_and_interp_chunk %>%
                data.table::as.data.table()
            
            sitevar_chunk <- summary_and_interp_chunk_dt[, .(
                site_code = data.table::first(site_code),
                var = data.table::first(var),
                val_err = if(use_sum)
                {
                    #the errors package uses taylor series expansion here.
                    #maybe implement some day.
                    sum(val_err, na.rm = TRUE)
                } else {
                    max(sd_or_0(val, na.rm = TRUE) / sqrt(.N),
                        mean(val_err, na.rm = TRUE))
                },
                val = if(use_sum) sum(val, na.rm = TRUE) else mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status)
            ), by = datetime] %>%
                as_tibble() %>%
                bind_rows(interp_only_chunk) %>%
                arrange(datetime)
        }
        

        sitevar_chunk <- populate_implicit_NAs(
            d = sitevar_chunk,
            interval = desired_interval)
        
        if(interpolate_method == 'linear'){

            d_split[[i]] <- ms_linear_interpolate(
                d = sitevar_chunk,
                interval = desired_interval,
                max_samples_to_impute = interpolate_limit)

        } else if(interpolate_method == 'nocb'){

            d_split[[i]] <- ms_nocb_interpolate(
                d = sitevar_chunk,
                interval = desired_interval,
                max_samples_to_impute = interpolate_limit)

        } else if(interpolate_method == 'nocb_mean'){

            d_split[[i]] <- ms_nocb_mean_interpolate(
                d = sitevar_chunk,
                interval = desired_interval,
                max_samples_to_impute = interpolate_limit)

        } else {

            d_split[[i]] <- ms_zero_interpolate(
                d = sitevar_chunk,
                interval = desired_interval,
                max_samples_to_impute = interpolate_limit)

        }
    }

    #recombine list of tibbles into single tibble
    d <- d_split %>%
        purrr::reduce(bind_rows) %>%
        arrange(site_code, var, datetime) %>%
        dplyr::select(datetime, site_code, var, val, ms_status, ms_interp, val_err)
    
    return(d)
}

