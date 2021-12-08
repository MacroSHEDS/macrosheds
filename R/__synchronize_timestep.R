synchronize_timestep <- function(d,
                                 desired_interval,
                                 impute_limit = 30){

    #d is a df/tibble with columns: datetime (POSIXct), site_code, var, val, ms_status
    #desired_interval [HARD DEPRECATED] is a character string that can be parsed by the "by"
    #   parameter to base::seq.POSIXt, e.g. "5 mins" or "1 day". THIS IS NOW
    #   DETERMINED PROGRAMMATICALLY. WE'RE ONLY GOING TO HAVE 2 INTERVALS,
    #   ONE FOR GRAB DATA AND ONE FOR SENSOR. IF WE EVER WANT TO CHANGE THEM,
    #   IT WOULD BE BETTER TO CHANGE THEM JUST ONCE HERE, RATHER THAN IN
    #   EVERY KERNEL
    #impute_limit [HARD DEPRECATED] is the maximum number of consecutive points to
    #   inter/extrapolate. it's passed to imputeTS::na_interpolate. THIS
    #   PARAMETER WAS REMOVED BECAUSE IT SHOULD ONLY VARY WITH DESIRED INTERVAL.

    #output will include a numeric binary column called "ms_interp".
    #0 for not interpolated, 1 for interpolated
    
    if(nrow(d) < 2 || sum(! is.na(d$val)) < 2){
        stop('no data to synchronize. bypassing processing.')
    }
    
    #split dataset by site and variable. for each, determine whether we're
    #   dealing with ~daily data or ~15min data. set rounding_intervals
    #   accordingly. This approach avoids OOM errors with giant groupings.
    d_split <- d %>%
        group_by(site_code, var) %>%
        arrange(datetime) %>%
        dplyr::group_split() %>%
        as.list()
    
    mode_intervals_m <- vapply(
        X = d_split,
        FUN = function(x) Mode(diff(as.numeric(x$datetime)) / 60),
        FUN.VALUE = 0)
    
    rounding_intervals <- case_when(
        is.na(mode_intervals_m) | mode_intervals_m > 12 * 60 ~ '1 day',
        mode_intervals_m <= 12 * 60 ~ '1 day') #TODO, TEMPORARY: switch this back
    # mode_intervals_m <= 12 * 60 ~ '15 min')
    
    for(i in 1:length(d_split)){
        
        sitevar_chunk <- d_split[[i]]
        
        n_dupes <- sum(duplicated(sitevar_chunk$datetime) |
                           duplicated(sitevar_chunk$datetime,
                                      fromLast = TRUE))
        
        #average values for duplicate timestamps
        if(n_dupes > 0){
            
            logwarn(msg = glue('{n} duplicate datetimes found for site: {s}, var: {v}',
                               n = n_dupes,
                               s = sitevar_chunk$site_code[1],
                               v = sitevar_chunk$var[1]),
                    logger = logger_module)
            
            sitevar_chunk_dt <- sitevar_chunk %>%
                mutate(val_err = errors(val),
                       val = errors::drop_errors(val)) %>%
                as.data.table()
            
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
                as_tibble() %>%
                mutate(val = errors::set_errors(val, val_err)) %>%
                select(-val_err)
            
            # sitevar_chunk <- sitevar_chunk %>%
            #     group_by(datetime) %>%
            #     summarize(site_code = first(site_code),
            #               var = first(var),
            #               val = mean(val, na.rm = TRUE),
            #               ms_status = numeric_any(ms_status)) %>%
            #     ungroup()
        }
        
        #round each site-variable tibble's datetime column to the desired interval.
        sitevar_chunk <- mutate(sitevar_chunk,
                                datetime = lubridate::round_date(
                                    x = datetime,
                                    unit = rounding_intervals[i]))
        
        #split chunk into subchunks. one has duplicate datetimes to summarize,
        #   and the other doesn't. both will be interpolated in a bit.
        to_summarize_bool <- duplicated(sitevar_chunk$datetime) |
            duplicated(sitevar_chunk$datetime,
                       fromLast = TRUE)
        
        summary_and_interp_chunk <- sitevar_chunk[to_summarize_bool, ]
        interp_only_chunk <- sitevar_chunk[! to_summarize_bool, ]
        
        if(nrow(summary_and_interp_chunk)){
            
            #summarize by sum for P, and mean for everything else
            # var_is_q <- drop_var_prefix(sitevar_chunk$var[1]) == 'discharge'
            var_is_p <- drop_var_prefix(sitevar_chunk$var[1]) == 'precipitation'
            
            summary_and_interp_chunk_dt <- summary_and_interp_chunk %>%
                mutate(val_err = errors(val),
                       val = errors::drop_errors(val)) %>%
                as.data.table()
            
            sitevar_chunk <- summary_and_interp_chunk_dt[, .(
                site_code = data.table::first(site_code),
                var = data.table::first(var),
                val_err = if(var_is_p)
                {
                    #the errors package uses taylor series expansion here.
                    #maybe implement some day.
                    sum(val_err, na.rm = TRUE)
                } else {
                    max(sd_or_0(val, na.rm = TRUE) / sqrt(.N),
                        mean(val_err, na.rm = TRUE))
                },
                val = if(var_is_p) sum(val, na.rm = TRUE) else mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status)
            ), by = datetime] %>%
                as_tibble() %>%
                mutate(val = set_errors(val, val_err)) %>%
                select(-val_err) %>%
                bind_rows(interp_only_chunk) %>%
                arrange(datetime)
            
            # sitevar_chunk <- summary_and_interp_chunk %>%
            #     group_by(datetime) %>%
            #         summarize(
            #             site_code = first(site_code),
            #             var = first(var),
            #             val = if(var_is_p)
            #                 {
            #                     sum(val, na.rm = TRUE)
            #                 # } else if(var_is_q){
            #                 #     max_ind <- which.max(val) #max() removes uncert
            #                 #     if(max_ind) val[max_ind] else NA_real_
            #                 } else {
            #                     mean(val, na.rm = TRUE)
            #                 },
            #             ms_status = numeric_any(ms_status)) %>%
            #         ungroup() %>%
            #     bind_rows(interp_only_chunk) %>%
            #     arrange(datetime)
        }
        
        sitevar_chunk <- populate_implicit_NAs(
            d = sitevar_chunk,
            interval = rounding_intervals[i])
        
        d_split[[i]] <- ms_linear_interpolate(
            d = sitevar_chunk,
            interval = rounding_intervals[i])
    }
    
    #recombine list of tibbles into single tibble
    d <- d_split %>%
        purrr::reduce(bind_rows) %>%
        arrange(site_code, var, datetime) %>%
        select(datetime, site_code, var, val, ms_status, ms_interp)
    
    return(d)
}
