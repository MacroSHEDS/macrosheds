library(macrosheds)
library(testthat)

dir.create('data/ms_test', showWarnings = FALSE)
macrosheds::ms_download_core_data(macrosheds_root = 'data/ms_test',
                                  domains = 'hbef',
                                  quiet = TRUE)

test_that('error is thrown for non sparial products', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                      spatial_product = 'fake_prod'),
                 'a spatial_product of ws_boundary, precip_gauge_locations, or stream_gauge_locations must be supplied')
})

test_that('one site_codes shapefiles is loaded', {
    
    expect_s3_class(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                         spatial_product = 'ws_boundary',
                                                         site_codes = 'w1'),
                    c('sf', 'data.frame'))
    
    expect_equal(nrow(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                           spatial_product = 'ws_boundary',
                                                           site_codes = 'w1')), 1)
})

test_that('one domains shapefiles is loaded', {
    
    expect_s3_class(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                         spatial_product = 'ws_boundary',
                                                         domains = 'hbef'),
                    c('sf', 'data.frame'))
    
    expect_equal(nrow(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                           spatial_product = 'ws_boundary',
                                                           domains = 'hbef')), 9)
})

test_that('multiple products throws an error', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = 'data/ms_test',
                                                         spatial_product = c('ws_boundary', 'precip_gauge_locations'),
                                                         domains = 'hbef'),
                    'only one spatial product can be loaded at a time')
})

test_that('ms_root must be correct', {
    expect_error(macrosheds::ms_load_spatial_product(macrosheds_root = 'fake_fake',
                                                      spatial_product = c('ws_boundary'),
                                                      domains = 'hbef'),
                 'No spatial products in macrosheds_root, check macrosheds_root is correct or files exist')
})

