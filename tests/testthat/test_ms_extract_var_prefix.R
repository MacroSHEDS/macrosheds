library(macrosheds)
library(testthat)


test_that('var prefix is character', {
    expect_type(ms_extract_var_prefix('GN_NO3_N'), 'character')
    expect_type(ms_extract_var_prefix('pm_sub_surf_permeability_sd'), 'character')
})

test_that('var prefix is of length 1', {
    expect_equal(length(ms_extract_var_prefix('GN_NO3_N')), 1)
})

test_that('var prefix is of nchar 2', {
    expect_equal(nchar(ms_extract_var_prefix('GN_NO3_N')), 2)
})

test_that("underscore didn't come along for the ride", {
    expect_no_match(ms_extract_var_prefix('GN_NO3_N'), '_')
})
