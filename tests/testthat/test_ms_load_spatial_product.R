library(macrosheds)
library(testthat)

#wd <- '../../data/ms_test' #superfluous files in the data/ directory causes problems for build.
wd <- '~/ssd2/ms_test' #so use a directory that works for your machine

options(timeout = 1200)
dir.create(wd, showWarnings = FALSE)
macrosheds::ms_download_core_data(macrosheds_root = wd,
                                  domains = 'hbef',
                                  quiet = TRUE)

test_that('error is thrown for non sparial products', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                      spatial_product = 'fake_prod'),
                 'Available `spatial_product`')
})

test_that('one site_codes shapefiles is loaded', {

    expect_s3_class(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                         spatial_product = 'ws_boundary',
                                                         site_codes = 'w1'),
                    c('sf', 'data.frame'))

    expect_equal(nrow(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                           spatial_product = 'ws_boundary',
                                                           site_codes = 'w1')), 1)
})

test_that('one domains shapefiles is loaded', {

    expect_s3_class(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                         spatial_product = 'ws_boundary',
                                                         domains = 'hbef'),
                    c('sf', 'data.frame'))

    expect_equal(nrow(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                           spatial_product = 'ws_boundary',
                                                           domains = 'hbef')), 9)
})

test_that('multiple products throws an error', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = wd,
                                                         spatial_product = c('ws_boundary', 'precip_gauge_locations'),
                                                         domains = 'hbef'),
                    'only one type of `spatial_product` can be loaded at a time')
})

test_that('ms_root must be correct', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = 'fake_fake',
                                                      spatial_product = c('ws_boundary'),
                                                      domains = 'hbef'),
                 'No data detected in macrosheds_root. This should')
})

