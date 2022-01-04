library(macrosheds)
library(testthat)


test_that('var is character', {
    expect_type(ms_drop_var_prefix('GN_NO3_N'), 'character')
    expect_type(ms_drop_var_prefix('pm_sub_surf_permeability_sd'), 'character')
})

test_that('var is of length 1', {
    expect_equal(length(ms_drop_var_prefix('GN_NO3_N')), 1)
})

test_that('var is correct nchar', {
    expect_equal(nchar(ms_drop_var_prefix('GN_NO3_N')), 2)
})

test_that("underscore didn't get removed", {
    expect_no_match(ms_drop_var_prefix('GN_NO3_N'), '_')
})
