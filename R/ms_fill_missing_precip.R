#' Fill gaps in precipitation gauge data
#'
#' Fill precipitation gauges based on relationship to surrounding gauges.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param precip \code{data.frame}. A \code{data.frame} or path to \code{data.frame} 
#'    in macrosheds format of precipitation gauge data 
#' @param precip_gauge a sf object or path to a sf object of precipitation gauge 
#'    locations
#' @param fill_method One of strongest_cor, closest_gauge, or distance_weight. See details
#' @param distance_threshold When distance_weight is selected, the distance in km 
#'    other gauges will be used to fill each gauge of interest
#' @param out_path Optional. path to save filled precipitation data
#' @param verbose Optional logical. Should information be printed to console, default TRUE. 
#' @return returns a \code{tibble} with filled precipitation data.
#' @details ms_fill_missing_precip uses other gauges in a network to fill gaps in 
#'    gauges when they are not reported data. Three different methods can be chosen.
#'    1) strongest_cor uses the gauge with the strongest correlation to the gauge with 
#'    missing data, and then uses a linear regression between the two gauges to fill
#'    in times of missing data. 2) closest_gauge uses a linear regression between
#'    the nearest gauge to the gauge with missing data. 3) distance_weight uses all
#'    gauges withing the selected distance_threshold and inverse distance weighting to
#'    fill the missing values. 







# TO DO: This currently fills all times when a single gauge in the dataset has data
# with data. This is becuase the table is pivoted and then those NAs are filled,
# this should probaly only fill NAs in the long dataset. 


# library(tidyverse)
# library(feather)
# library(macrosheds)
# library(foreach)
# 
# 
# fils <- list.files('../data_processing/data/lter/santa_barbara/derived/precipitation__ms002/', 
#                    full.names = TRUE)
# precip <- purrr::map_dfr(fils, feather::read_feather) %>%
#     filter(! site_code == 'BuelltonFireStation233')
# pgauge_path <- list.files('../data_processing/data/lter/santa_barbara/derived/precip_gauge_locations__ms004/', full.names = TRUE)
# precip_gauge <- purrr::map_dfr(pgauge_path, sf::st_read) %>%
#     filter(! site_code %in% c('BuelltonFS233', 'Nojoqui236'))


ms_fill_missing_precip <- function(precip,
                                   precip_gauge,
                                   fill_method = 'strongest_cor',
                                   distance_threshold = NULL,
                                   out_path = NULL,
                                   verbose = TRUE){
    
    #### Load in data is file path supplied ####

    # Load in precipitation gauge locations
    if(! inherits(precip_gauge, 'sf')){
        rg_path <- list.files(precip_gauge, full.names = TRUE)
        rg <- try(purrr::map_dfr(rg_path, sf::st_read))
        
        if(inherits(rg, 'try-error')){
            stop('precip_gauge file failed to load, check file path is correct')
        }
    } else{
        rg <- precip_gauge
    }
    
    # Load in precipitation data 
    if(! inherits(precip, c('data.frame', 'tbl', 'tibble', 'tbl_df'))){
        precip_path <- list.files(precip, full.names = TRUE)
        precip <- try(purrr::map_dfr(precip_path, feather::read_feather))
        
        if(inherits(precip, 'try-error')){
            stop('precip file failed to load, check file path is correct')
        }
    } else{
        precip <- precip
    }
    
    if('val_err' %in% names(precip)){
        errors::errors(precip$val) <- precip$val_err
        precip <- precip %>%
            select(-val_err)
    }
    

    ### Checks ####
    if(! fill_method %in% c('distance_weight', 'closest_gauge', 'strongest_cor')){
        print('fill_method must be one of: distance_weight, closest_gauge, strongest_cor')
    }
    
    if(fill_method == 'distance_weight' && is.null(distance_threshold)) {
        stop('If distance_weight is the selected fill_method, a distance_threshold in km must be supplied')
    }
    
    precip_var_name <- unique(precip$var)
    if(! length(precip_var_name) == 1){
        stop('precipitation tables contain multiple variables, only one precipitation variables is accepted')
    }
    
    if(! length(unique(rg$site_code)) == length(unique(rg$site_code))){
        stop('the precip_gauge file contain duplicate entries for the same gauge')
    }
    
    if(! all(rg$site_code %in% unique(precip$site_code))){
        missing_gauge <- rg$site_code[! rg$site_code %in% unique(precip$site_code)]
        stop(paste0('a precip gauge location exists in the precip gauge file for the gauge(s): ', 
                    paste0(missing_gauge, collapse = ', '),
                    ' but no corresponding data exists in the precip file,',
                    ' either add data to the precip file for these gauges or',
                    ' remove the gauge from the precip_gauge file'))
    }
    if(! all(unique(precip$site_code) %in% rg$site_code)){
        missing_gauge <- unique(precip$site_code)[!unique(precip$site_code) %in% rg$site_code]
        stop(paste0('data exists in the precip file for the gauge(s): ', paste0(missing_gauge, collapse = ', '),
                    ' but have no corresponding location in the precip_gauge file,',
                    ' either add these gauge(s) to the precip_gauge file or remove corresponding data from the preicp file'))
    }
    
    
    status_cols <- precip %>%
        select(datetime, site_code, ms_status, ms_interp) %>%
        group_by(datetime, site_code) %>%
        sm(summarize(
            ms_status = numeric_any(ms_status),
            ms_interp = numeric_any(ms_interp)))
    
    var_name <- unique(precip$var)
    if(length(var_name) > 1 && verbose){
        print('precip table contains multiple var names, the first will be selected to apply to whole table')
        var_name <- var_name[1]
    }
 
    day_durations_byproduct <- datetimes_to_durations(
        datetime_vec = precip$datetime,
        variable_prefix_vec = ms_extract_var_prefix(precip$var),
        unit = 'days',
        installed_maxgap = 2,
        grab_maxgap = 30)
    
    precip$val[is.na(day_durations_byproduct)] <- NA
    
    precip <- precip %>%
        ungroup() %>%
        select(-ms_status, -ms_interp, -var) %>%
        tidyr::pivot_wider(names_from = site_code,
                           values_from = val) %>%
        arrange(datetime)
    
    # Need to retain the origonal data wile also filling it (to avoid filling 
    # values with other filled values)
    precip_final <- precip

    just_data <- precip %>%
        select(-datetime) %>%
        mutate(across(.fns = as.numeric))
        
    missing_data <- just_data
    missing_data[!is.na(missing_data)] <- 1 
    missing_data <- missing_data %>%
        summarise(across(.fns =  ~ sum(.x, na.rm = TRUE)))
        
        
    sites_with_missing <- names(missing_data) %in% !missing_data == nrow(just_data)
        
        for(i in 1:length(missing_data)){
            gauge <- names(missing_data)[i]
            
            this_gauge <- filter(rg, site_code == !!gauge)
            
            # Determine sites with enough fill data 
            site_with_fill_data <- just_data %>%
                filter(is.na(.data[[!!gauge]]))
            
            missing_vals <- nrow(site_with_fill_data)
            
            site_with_fill_data[!is.na(site_with_fill_data)] <-1 
            site_with_fill_data <- site_with_fill_data %>%
                summarise(across(.fns =  ~ sum(.x, na.rm = TRUE)))
            site_with_fill_data <- site_with_fill_data/missing_vals
            
            site_with_fill_data <- tibble(site = names(site_with_fill_data),
                                          val = as.numeric(site_with_fill_data[1,])) %>%
                filter(val >= 0.75) %>%
                pull(site)
            
            if(fill_method == 'distance_weight'){
                rg_ <- rg
                
                rg_$distance <- sf::st_distance(this_gauge$geometry, rg$geometry)[,]
                
                rg_ <- rg_ %>%
                    mutate(distance = as.numeric(distance)/1000) %>%
                    filter(distance <= !!distance_threshold) %>%
                    as.data.frame() %>%
                    select(-geometry) %>%
                    filter(site_code !=  !!gauge)
                
                fill_dates <- precip %>%
                    filter(is.na(.data[[gauge]])) %>%
                    select(datetime, !!gauge, !!rg_$site_code)
                
                fill_dates_nrow <- nrow(fill_dates)
                
                for(r in 1:fill_dates_nrow){
                    
                    ts_fill_gauge <- names(fill_dates)[!is.na(fill_dates[r,])]
                    
                    rg_ts <- rg_ %>%
                        filter(site_code %in% !! ts_fill_gauge)
                    
                    if(nrow(rg_ts) == 0) next
                    
                    rg_ts$total_dis <- sum(rg_ts$distance)
                    
                    rg_ts <- rg_ts %>%
                        mutate(weight = distance/total_dis)
                    
                    weighted_val <- sm(fill_dates[r,] %>%
                        select(-datetime) %>%
                        tidyr::pivot_longer(cols = everything(), names_to = 'site_code') %>%
                        filter(!is.na(value)) %>%
                        left_join(rg_ts, by = 'site_code') %>%
                        mutate(prop_val = value * weight))
                    
                    fill_dates[r,gauge] <- sum(weighted_val$prop_val)
                }
                
                new_name <- paste0('filled_', gauge)
                fill_data <- fill_dates %>%
                    select(datetime, !!gauge)  %>%
                    rename(!!new_name := !!gauge)
                
                precip_final <- precip_final %>%
                    full_join(., fill_data, by = 'datetime') %>%
                    mutate(!!gauge := ifelse(is.na(.data[[gauge]]), .data[[new_name]], .data[[gauge]])) %>%
                    select(-!!new_name)
                
                if(verbose){
                    print(glue::glue('{n} records filled for {g} using gauges: {fg}',
                               n = fill_dates_nrow,
                               g = gauge,
                               fg = paste0(rg_$site_code, collapse = ', ')))
                }
                next
                
            }
            if(fill_method == 'closest_gauge'){
                rg_ <- rg
                
                rg_$distance <- sf::st_distance(this_gauge$geometry, rg$geometry)[,]
                rg_ <- rg_ %>%
                    filter(site_code %in% !!site_with_fill_data)
                
                fill_gauge <- filter(rg_, as.numeric(rg_$distance) == !!min(as.numeric(rg_$distance))) %>%
                    pull(site_code)
            }
            
            if(fill_method == 'strongest_cor'){
                all_r <- tibble()
                for(s in 1:length(missing_data)){
                    comp_gauge <- names(missing_data)[s]
                    r_val <- try(summary(lm(as.numeric(get(gauge)) ~ as.numeric(get(comp_gauge)), precip))$r.squared, silent = TRUE)
                    
                    if(inherits(r_val, 'try-error')) next
                    
                    r_tib <- tibble(site_code = comp_gauge,
                                    r_val = r_val)
                    all_r <- rbind(all_r, r_tib)
                    
                }
                
                all_r <- all_r %>%
                    filter(site_code %in% !!site_with_fill_data) 
                
                if(nrow(all_r) == 0){
                    print(glue::glue('Unable to fill records for {g} becuase no single guage',
                                     ' has coverage for more than 75% of the days when this guage',
                                     ' was down. Try using distance weighting',
                                     g = gauge))
                    next
                }
                
                fill_gauge <- all_r %>%
                    filter(r_val == !!max(all_r$r_val, na.rm = TRUE)) %>%
                    pull(site_code)
                
            }
            
            mod <- lm(as.numeric(get(gauge)) ~ as.numeric(get(fill_gauge)), data = precip)
            ab <- as.list(mod$coefficients)
            
            data_to_fill <- precip %>%
                select(datetime, !!gauge, !!fill_gauge) %>%
                filter(is.na(.data[[!!gauge]]))
            
            # Estimate fill values
            d_fill <- sm(ab$`as.numeric(get(fill_gauge))` * data_to_fill[fill_gauge] + ab$`(Intercept)`)
            
            sm(d_fill[d_fill < 0] <- 0)
            
            data_to_fill[gauge] <- d_fill
            
            # Using function will result in 0 days being reported as the intercept
                # Correct here 
            new_name <- paste0('filled_', gauge)
            data_to_fill <- data_to_fill %>%
                mutate(!!gauge := ifelse(as.numeric(.data[[!!fill_gauge]]) == 0, 0, .data[[!!gauge]])) %>%
                rename(!!new_name := !!gauge) %>%
                select(datetime, !!new_name)
            
            
            precip_final <- precip_final %>%
                full_join(., data_to_fill, by = 'datetime') %>%
                mutate(!!gauge := ifelse(is.na(.data[[gauge]]), .data[[new_name]], .data[[gauge]])) %>%
                select(-!!new_name)
            
            if(verbose){
                print(glue::glue('{n} records filled for {g} using {fg}',
                           n = nrow(data_to_fill),
                           g = gauge,
                           fg = fill_gauge))
            }
            
        }
    
    # precip <- precip_
    
    pivot_names <- names(precip_final)
    pivot_names <- pivot_names[! pivot_names %in% 'datetime']
    precip_final <- precip_final %>%
        tidyr::pivot_longer(cols = !!pivot_names, 
                     names_to = 'site_code',
                     values_to = 'val') %>%
        full_join(status_cols, by = c('site_code', 'datetime')) %>%
        mutate(ms_interp = ifelse(is.na(ms_interp), 1, ms_interp)) %>%
        mutate(ms_status = ifelse(is.na(ms_status), 0, ms_status)) %>%
        arrange(site_code, datetime) %>%
        mutate(var = !!var_name)
    
    if(is.null(out_path)){
        return(precip_final)
    } else{
        feather::write_feather(precip_final, outpath)
    }

}

# output <- ms_fill_missing_precip(precip = precip,
#                                  precip_gauge = precip_gauge,
#                                  out_path = NULL,
#                                  fill_method = 'closest_gauge',
#                                  # distance_threshold = 10,
#                                  # distance_threshold = NULL,
#                                  verbose = T)
# 
# 
# 
# precip_test <- precip %>%
#     ungroup() %>%
#     select(-ms_status, -ms_interp, -var) %>%
#     tidyr::pivot_wider(names_from = site_code,
#                        values_from = val) %>%
#     arrange(datetime) 
# 
# pivot_names <- names(precip_test)
# pivot_names <- pivot_names[! pivot_names %in% 'datetime']
# 
# precip_test <- precip_test %>%
#     tidyr::pivot_longer(cols = !!pivot_names, 
#                  names_to = 'site_code',
#                  values_to = 'val') %>%
#     arrange(site_code, datetime) 
# 
# ggplot(precip, aes(datetime, val, col = site_code)) +
#     geom_line()
# 
# ggplot(output, aes(datetime, val, col = site_code)) +
#     geom_line()
# 
# ggplot(precip_test, aes(datetime, val, col = site_code)) +
#     geom_line()
# 
# 
# 
# data_to_fill_test <- data_to_fill %>%
#     select(datetime, BaronRanch262)
# 
# look=full_join(precip_test, data_to_fill_test, by = 'datetime')
# 
# look <- look %>%
#     filter(is.na(BaronRanch262.x))
