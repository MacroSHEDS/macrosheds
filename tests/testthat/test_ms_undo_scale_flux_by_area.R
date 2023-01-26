library(macrosheds)
library(testthat)
library(lubridate)


w1 <- tibble(datetime = c(lubridate::ymd('2001-01-01', '2001-01-02', '2001-01-03')),
             site_code = 'w1',
             var = 'GN_Ca',
             val = c(1,2,3),
             ms_status = 0,
             ms_interp = 0,
             val_err = .0001)

test_that('converstin from kg/ha/d to kg/d is results in a larger value', {
    expect_gt(sum(macrosheds::ms_undo_scale_flux_by_area(d = w1)$val), sum(w1$val))
})

# test_that('a site_data path that is not real throws error', {
#     expect_error(macrosheds::ms_undo_scale_flux_by_area(d = w1, site_data = 'fake'),
#                  'please enter a correct path to site_data file or omit argument')
# })

test_that('a tibble is returned when site_data is supplied', {
    expect_s3_class(macrosheds::ms_undo_scale_flux_by_area(d = w1),
                    'tbl_df')
})

test_that('the value for 2001-01-01 is ~14.0619', {
    expect_equal(round(pull(macrosheds::ms_undo_scale_flux_by_area(d = w1)[1, 4]), 4),
                 14.0619)
})

test_that("the value for 2001-01-01 is ~14.0619 when site_data is passed", {
    expect_equal({
        out <- macrosheds::ms_undo_scale_flux_by_area(d = w1)
        round(pull(out[1, 4]), 4)
    }, 14.0619)
})
