#' summarize macrosheds data to specified time intervals
#'
#' ms_synchronize_timestep both summarizes macrosheds to weekly, monthly, and 
#' annual time scales and interpolates macrosheds data to a finer time step such 
#' as sub daily 
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param d \code{date.frame}. A macrosheds \code{dataframe} downloaded using 
#'     \code{download_ms_core_data()}
#' @param desired_interval character. Input that can be parsed by the "by"
#'     parameter to base::seq.POSIXt, e.g. "5 mins", "1 day", "1 month" 
#' @param impute_limit numeric. The maximum number of consecutive points to
#'     inter/extrapolate. it's passed to imputeTS::na_interpolate.
#' @param summary_fun One of "mean" or "sum". The function that will be used to 
#'     summarize data if desired_interval is larger than the input data in d
#' @param quiet logical. Should warnings be printed to console 
#' @return returns a \code{tibble} with data in the new desired_interval with a 
#'     ms_interp column indicating if a value was interpolated 
#' @details ms_synchronize_timestep is used to convert data into a finer
#'     temporal resolution, such as taking a weekly grab sample and interpolating 
#'     the values to have daily data, or to aggregate data into large intervals 
#'     such as monthly and annual sums or means. Macrosheds data undergoes temporal
#'     synchronization in our data processing pipeline. Our data is aggregated/imputed 
#'     to a daily time step for all variables and the impute limit for discharge and 
#'     precipitation is 3 days and for chemistry variables 15 days.
#' @export

ms_synchronize_timestep <- function(d,
                                    desired_interval,
                                    impute_limit,
                                    summary_fun,
                                    quiet = FALSE){

    # Checks 
    if(nrow(d) < 2 || sum(! is.na(d$val)) < 2){
        stop('no data to synchronize')
    }
    
    if(! missing(summary_fun) && ! summary_fun %in% c('sum', 'mean')) {
        stop('summary_fun must be either sum or mean')
    }
    
    #split dataset by site and variable.
    d_split <- d %>%
        group_by(site_code, var) %>%
        arrange(datetime) %>%
        dplyr::group_split() %>%
        as.list()
    
    mode_intervals_m <- vapply(
        X = d_split,
        FUN = function(x) Mode(diff(as.numeric(x$datetime)) / 60),
        FUN.VALUE = 0)
    
    rounding_intervals <- desired_interval

    for(i in 1:length(d_split)){

        sitevar_chunk <- d_split[[i]]
        
        n_dupes <- sum(duplicated(sitevar_chunk$datetime) |
                           duplicated(sitevar_chunk$datetime,
                                      fromLast = TRUE))
        
        # average/sum values for duplicate timestamps
        if(n_dupes > 0){
            
            msg <- glue('{n} duplicate datetimes found for site: {s}, var: {v}',
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
        sitevar_chunk <- mutate(sitevar_chunk,
                                datetime = lubridate::round_date(
                                    x = datetime,
                                    unit = rounding_intervals))
        
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
            interval = rounding_intervals)
        
        d_split[[i]] <- ms_linear_interpolate(
            d = sitevar_chunk,
            interval = rounding_intervals,
            max_samples_to_impute = impute_limit)
    }

    #recombine list of tibbles into single tibble
    d <- d_split %>%
        purrr::reduce(bind_rows) %>%
        arrange(site_code, var, datetime) %>%
        select(datetime, site_code, var, val, ms_status, ms_interp, val_err)
    
    return(d)
}

