library(macrosheds)
library(testthat)

test_that('all vars load', {
    expect_s3_class(ms_load_variables(var_set = 'timeseries'), 'data.frame')
    expect_s3_class(ms_load_variables(var_set = 'ws_attr'), 'data.frame')
    expect_s3_class(ms_load_variables(var_set = 'timeseries_by_site'), 'data.frame')
})

test_that('error for illegal entry', {
    expect_error(ms_load_variables(var_set = 'chili'), regexp = 'must be')
})
