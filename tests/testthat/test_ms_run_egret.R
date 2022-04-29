library(macrosheds)
library(lubridate)
library(testthat)


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
                 'stream_chemistry must be a macrosheds file with the columns site_code, 
             datetime, var, and, val')
    
    expect_error(macrosheds::ms_run_egret(stream_chemistry = ms_table_c,
                                          discharge = ms_table_d_fake),
                 'discharge must be a macrosheds file with the columns site_code, 
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
