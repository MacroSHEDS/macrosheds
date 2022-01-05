library(macrosheds)
library(testthat)


w1 <- tibble(datetime = c(ymd('2001-01-01', '2001-01-02', '2001-01-03')),
             site_code = 'w1',
             var = 'GN_Ca',
             val = c(1,2,3),
             ms_status = 0,
             ms_interp = 0,
             val_err = .0001)

test_that('converstin from kg/ha/d to kg/d is results in a larger value', {
    expect_gt(sum(macrosheds::ms_undo_scale_flux_by_area(d = w1)$val), sum(w1$val))
})

test_that('a site_data path that is not real throws error', {
    expect_error(macrosheds::ms_undo_scale_flux_by_area(d = w1, site_data = 'fake'),
                 'please enter a correct path to site_data file or omit argument')
})
