library(macrosheds)
library(lubridate)
library(testthat)

datetime_string <- c(ymd('2000-01-01', '2000-01-02', '2000-01-03',
                         '2000-01-04', '2000-01-05', '2000-01-06'))

no3 <- tibble(datetime = datetime_string,
              site_code = 'w1', 
              var = 'GN_NO3', 
              val = c(seq(0.2, 1.2, by = 0.2)),
              ms_status = 0,
              ms_interp = 0,
              val_err = 0.01)

cl <- tibble(datetime = datetime_string,
             site_code = 'w1', 
             var = 'GN_Cl', 
             val = c(seq(0.2, 1.2, by = 0.2)),
             ms_status = 0,
             ms_interp = 0,
             val_err = 0.01)

test_that('metric conversions are correct', {
    
    ug_results <- macrosheds::ms_conversions(d = cl,
                                             convert_units_from = 'ug/l',
                                             convert_units_to = 'mg/l')
    expect_equal(results_hbef, 
                 c('w1', 'w2'))
    
    results_santee <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                                  prodname = 'discharge',
                                                  site_codes = c('WS77', 'w7'),
                                                  warn = F) %>%
        pull(site_code) %>%
        unique()
    expect_equal(results_santee, 
                 c('w7', 'WS77'))
})