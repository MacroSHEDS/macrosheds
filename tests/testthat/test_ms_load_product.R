library(macrosheds)
library(testthat)

#wd <- '../../data/ms_test' #superfluous files in the data/ directory causes problems for build.
wd <- '~/ssd2/ms_test' #so use a directory that works for your machine

options(timeout = 1200)

#### data for test
dir.create(wd, showWarnings = FALSE)
macrosheds::ms_download_core_data(macrosheds_root = wd,
                                  domains = c('hbef', 'hjandrews', 'boulder',
                                              'santee'),
                                  quiet = TRUE)
macrosheds::ms_download_core_data(macrosheds_root = wd,
                                  domains='santee',
                                  version = 1,
                                  quiet = TRUE)
macrosheds::ms_download_ws_attr(macrosheds_root = wd, dataset = 'summaries')
macrosheds::ms_download_ws_attr(macrosheds_root = wd, dataset = 'time series', omit_climate_data = T)
macrosheds::ms_download_ws_attr(macrosheds_root = wd, dataset = 'CAMELS summaries')

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

test_that('errors are printed as expected', {

    expect_error(ms_load_product(macrosheds_root = 'fake_dir_test',
                                 prodname = 'stream_chemistry'),
                 'macrosheds_root does not exist. This should')

    # expect_error(ms_load_product(macrosheds_root = '~',
    #                              prodname = 'ws_attr_timeseries:landcover'),
    #              regexp = 'No file found for')
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
                 c('PO4_P', 'temp'))
})

test_that('loading ws attr summaries works', {

    r = macrosheds::ms_load_product(macrosheds_root = wd,
                    prodname = 'ws_attr_summaries',
                    domains = c('hjandrews', 'hbef'),
                    filter_vars = c('PO4_P', 'temp')) #ignored

    expect_length(unique(r$domain), 2)

    r = macrosheds::ms_load_product(macrosheds_root = wd,
                    prodname = 'ws_attr_CAMELS_summaries',
                    domains = c('hjandrews', 'hbef'))

    expect_length(unique(r$domain), 2)
})

test_that('loading ws attr timeseries works', {

    r = macrosheds::ms_load_product(macrosheds_root = wd,
                    prodname = 'ws_attr_timeseries:landcover',
                    domains = c('hjandrews', 'hbef'))

    expect_true('pctCellErr' %in% colnames(r))
})

test_that('misspecified network produces error', {

    expect_error(macrosheds::ms_load_product(macrosheds_root = wd,
                    prodname = 'ws_attr_timeseries:landcover',
                    domains = c('hjandrews', 'hbef', 'donkey')),
                 regexp = 'Unknown')
})

test_that('v1 and v2 load properly', {

    d1 <- macrosheds::ms_load_product(macrosheds_root = wd,
                                      prodname = 'discharge',
                                      domains = 'santee',
                                      version = 1,
                                      warn = FALSE)
    expect_gt(nrow(d1), 76000)
    expect_contains(colnames(d1), 'grab_sample')

    d2 <- macrosheds::ms_load_product(macrosheds_root = wd,
                                      prodname = 'discharge',
                                      domains = 'santee',
                                      version = 2,
                                      warn = FALSE)
    expect_gt(nrow(d2), 76000)
})

test_that('warnings and errors are properly issued.', {

    expect_error(
        macrosheds::ms_load_product(
            macrosheds_root = wd,
            prodname = 'discharge',
            domains = 'santee',
            version = 3,
            warn = FALSE
        ),
        'Version 3 data were'
    )

    expect_error(
        macrosheds::ms_load_product(
            macrosheds_root = wd,
            prodname = 'discharge',
            domains = 'santee',
            version = 'gg',
            warn = FALSE
        ),
        'must be a positive integer'
    )

    expect_warning(
        macrosheds::ms_load_product(
            macrosheds_root = wd,
            prodname = 'discharge',
            domains = 'santee',
            version = '1'
        ),
        'Data from MacroSheds v1 were requested'
    )

    expect_error(
        macrosheds::ms_load_product(
            macrosheds_root = wd,
            prodname = 'ws_attr_summaries',
            domains = 'santee',
            version = '1',
            warn = FALSE
        ),
        'No file found for'
    )

})
