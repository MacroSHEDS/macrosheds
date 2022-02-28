library(macrosheds)
library(testthat)

#testthat changes the working directory to r_package/tests/testthat
wd <- '../../data/ms_test'

#### data for test
dir.create(wd, showWarnings = FALSE)
macrosheds::ms_download_core_data(macrosheds_root = wd,
                                  domains = c('hbef', 'hjandrews', 'boulder',
                                              'santee'),
                                  quiet = TRUE)
print(getwd())



test_that('the correct domains are loaded in', {
    
    results_hbef <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                prodname = 'discharge',
                                domains = c('hbef'),
                                warn = F) %>%
        pull(site_code) %>%
        unique()
     expect_equal(results_hbef, 
                c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9'))
    
     results_santee <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                 prodname = 'discharge',
                                                 domains = c('santee'),
                                                 warn = F) %>%
         pull(site_code) %>%
         unique()
    expect_equal(results_santee, 
                 c('WS77', 'WS78', 'WS79', 'WS80'))
    
    results_2_doms <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                  prodname = 'discharge',
                                                  domains = c('santee', 'hbef'),
                                                  warn = F) %>%
        pull(site_code) %>%
        unique()
    expect_equal(results_2_doms, 
                 c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8', 'w9', 'WS77', 'WS78', 'WS79', 'WS80'))
})

test_that('the correct sites are loaded in', {
    
    results_hbef <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                prodname = 'discharge',
                                                site_codes = c('w1', 'w2'),
                                                warn = F) %>%
        pull(site_code) %>%
        unique()
    expect_equal(results_hbef, 
                 c('w1', 'w2'))
    
    results_santee <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                  prodname = 'discharge',
                                                  site_codes = c('WS77', 'w7'),
                                                  warn = F) %>%
        pull(site_code) %>%
        unique()
    expect_equal(results_santee, 
                 c('w7', 'WS77'))
})

test_that('the correct sites and domains are loaded in', {
    
    results_hbef <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                prodname = 'discharge',
                                                domains = 'santee',
                                                site_codes = c('w1', 'w2'),
                                                warn = F) %>%
        pull(site_code) %>%
        unique()
    expect_equal(results_hbef, 
                 c('WS77', 'WS78', 'WS79', 'WS80', 'w1', 'w2'))
})

test_that('directory error is printed', {
    expect_error(ms_load_product(macrosheds_root = 'fake_dir_test', 
                                 prodname = 'stream_chemistry'),
                 'macrosheds_root does not exist, please ensure correct directory is supplied')
})

test_that('filter_vars filters the correct vars', {
    results_vars <- macrosheds::ms_load_product(macrosheds_root = wd, 
                                                prodname = 'stream_chemistry',
                                                domains = c('hjandrews', 'hbef'),
                                                filter_vars = c('PO4_P', 'temp'),
                                                warn = F) %>%
        pull(var) %>%
        unique()
    expect_equal(results_vars, 
                 c('GN_PO4_P', 'GN_temp',  'IS_temp'))
})
