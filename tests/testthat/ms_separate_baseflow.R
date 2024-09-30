library(macrosheds)
library(testthat)
library(lubridate)

ms_table_d <- tibble(datetime = seq.Date(from = lubridate::ymd('2000-01-01'), to = lubridate::ymd('2000-12-31'), by = '1 day'),
                     site_code = 'non_ms', 
                     var = c('IS_discharge'), 
                     val = runif(366, 1, 16),
                     ms_status = 0,
                     ms_interp = 0,
                     val_err = 0)

fake_tibe <- tibble('date' = 1:10,
                    'discharge' = 1:10)


test_that('baseflow is seperated for a macrosheds discharge files', {
    
    expect_s3_class(ms_separate_baseflow(discharge = ms_table_d), 'tbl_df')
})

test_that('None macrosheds structures are expected', {
    expect_error(ms_separate_baseflow(fake_tibe))
})
