library(macrosheds)
library(testthat)

test_that('non-macrosheds runs secsfully', {
    
    ms_table_c <- tibble(datetime = seq.Date(from = lubridate::ymd('2000-01-01'), to = lubridate::ymd('2000-12-31'), by = '1 day'),
                         site_code = c('non_ms'), 
                         var = c('GN_Cl'), 
                         val = c(rep(c(seq(1, 5, by = 1), rev(seq(1, 5, by = 1))), 36), 1, 2, 3, 4, 5, 6),
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_d <- tibble(datetime = seq.Date(from = lubridate::ymd('2000-01-01'), to = lubridate::ymd('2000-12-31'), by = '1 day'),
                         site_code = 'non_ms', 
                         var = c('IS_discharge'), 
                         val = runif(366, 1, 16),
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    site_data <- tibble(site_code = 'non_ms',
                        ws_area_ha = 10,
                        latitude = 40.565763,
                        longitude = -105.043905)
    
    egret_results <- macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                              discharge = ms_table_d,
                                              site_data = site_data,
                                              prep_data = FALSE)
    
    expect_s3_class(egret_results, 'egret')
})

test_that('macrosheds sites runs successfully', {
    
    ms_table_c <- tibble(datetime = seq.Date(from = lubridate::ymd('2000-01-01'), to = lubridate::ymd('2000-12-31'), by = '1 day'),
                         site_code = c('w1'), 
                         var = c('GN_Cl'), 
                         val = c(rep(c(seq(1, 5, by = 1), rev(seq(1, 5, by = 1))), 36), 1, 2, 3, 4, 5, 6),
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_d <- tibble(datetime = seq.Date(from = lubridate::ymd('2000-01-01'), to = lubridate::ymd('2000-12-31'), by = '1 day'),
                         site_code = 'w1', 
                         var = c('IS_discharge'), 
                         val = runif(366, 1, 16),
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    egret_results <- macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                              discharge = ms_table_d,
                                              prep_data = FALSE)
    
    expect_s3_class(egret_results, 'egret')
})

test_that('tables are checked for ms format', {
    
    ms_table_c <- tibble(datetime = lubridate::ymd('2000-01-01'),
                         site_code = 'w1', 
                         var = c('GN_Cl'), 
                         val = 0.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_c_fake <- tibble(datetime = lubridate::ymd('2000-01-01'),
                              site_code = 'w1', 
                              CL = 0.2)
    
    ms_table_d <- tibble(datetime = lubridate::ymd('2000-01-01'),
                         site_code = 'w1', 
                         var = c('GN_discharge'), 
                         val = 1.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_d_fake <- tibble(datetime = lubridate::ymd('2000-01-01'),
                              site_code = 'w1', 
                              Q = 1)
    
    expect_error(macrosheds::ms_run_egret(stream_chemistry = ms_table_c_fake,
                                          discharge = ms_table_d),
                 'stream_chemistry must be a data.frame in MacroSheds format with the columns site_code, 
             datetime, var, and, val')
    
    expect_error(macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                          discharge = ms_table_d_fake),
                 'discharge must be a data.frame in MacroSheds format with the columns site_code, 
             datetime, var, and, val')
    
})

test_that('multi-site error is thrown', {
    
    ms_table_c <- tibble(datetime = c(lubridate::ymd('2000-01-01'), lubridate::ymd('2000-01-02')),
                         site_code = c('w1', 'w2'), 
                         var = 'GN_Cl', 
                         val = 0.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_d <- tibble(datetime = lubridate::ymd('2000-01-01'),
                         site_code = 'w1', 
                         var = c('GN_discharge'), 
                         val = 1.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    expect_error(macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                          discharge = ms_table_d),
                 'Only one site can be run in EGRET at a time')
})

test_that('multi-variable error is thrown', {
    
    ms_table_c <- tibble(datetime = c(lubridate::ymd('2000-01-01'), lubridate::ymd('2000-01-02')),
                         site_code = c('w1', 'w1'), 
                         var = c('GN_Cl', 'GN_Na'), 
                         val = 0.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    
    ms_table_d <- tibble(datetime = lubridate::ymd('2000-01-01'),
                         site_code = 'w1', 
                         var = c('GN_discharge'), 
                         val = 1.2,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0)
    expect_error(macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                          discharge = ms_table_d),
                 'Only one chemistry variable can be run at a time.')
})


