library(macrosheds)
library(lubridate)
library(testthat)

test_no_data <- tibble(datetime = ymd('2000-01-01'),
                       site_code = 'w1',
                       var = 'GN_P',
                       val = 1,
                       ms_status = 0,
                       ms_interp = 0,
                       val_err = 0.01)

test_d <- tibble(datetime = seq.Date(ymd('2000-01-01'), ymd('2001-12-31'), 
                                     by = '1 day'),
                 site_code = 'w1',
                 var = 'GN_P',
                 val = 1,
                 ms_status = 0,
                 ms_interp = 0,
                 val_err = 0.01)

test_d_interp <- tibble(datetime = seq.Date(ymd('2000-01-01'), ymd('2001-12-31'), 
                                            by = '1 week'),
                        site_code = 'w1',
                        var = 'GN_P',
                        val = rep(c(1,2,5,7,3), 21),
                        ms_status = 0,
                        ms_interp = 0,
                        val_err = 0.01)

test_that('tables with less than 2 samples throws an error', {
    expect_error(ms_synchronize_timestep(d = test_no_data, 
                                         desired_interval = '1 day',
                                         impute_limit = 10),
                 'no data to synchronize')
})

test_that('summary_fun error thrown', {
    expect_error(ms_synchronize_timestep(d = test_d, 
                                         desired_interval = '1 month',
                                         impute_limit = 10,
                                         summary_fun = '1 month'),
                 'summary_fun must be either sum or mean')
})

test_that('summary to year works', {
    expect_s3_class(ms_synchronize_timestep(d = test_d, 
                                            desired_interval = '1 year',
                                            summary_fun = 'mean'),
                    'data.frame')
    
    expect_equal(nrow(ms_synchronize_timestep(d = test_d, 
                                              desired_interval = '1 year',
                                              summary_fun = 'mean')),
                 2)
})

test_that('summary to month works', {
    expect_s3_class(ms_synchronize_timestep(d = test_d, 
                                            desired_interval = '1 month',
                                            summary_fun = 'mean'),
                    'data.frame')
    
    expect_equal(nrow(ms_synchronize_timestep(d = test_d, 
                                              desired_interval = '1 month',
                                              summary_fun = 'mean')),
                 24)
})

test_that('interpoltion works', {
    expect_s3_class(ms_synchronize_timestep(d = test_d_interp, 
                                            desired_interval = '1 day',
                                            impute_limit = 10),
                    'data.frame')
    
    expect_equal(nrow(ms_synchronize_timestep(d = test_d_interp, 
                                              desired_interval = '1 day',
                                              impute_limit = 10)),
                 729)
})

test_that('sub-daily interpoltion works', {
    expect_s3_class(ms_synchronize_timestep(d = test_d, 
                                            desired_interval = '15 min',
                                            impute_limit = 96),
                    'data.frame')
    
    expect_equal(nrow(ms_synchronize_timestep(d = test_d, 
                                              desired_interval = '15 min',
                                              impute_limit = 96)),
                 70081)
})
