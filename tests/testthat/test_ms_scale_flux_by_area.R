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

test_that('undo_scale followed by scale results in ~identity', {
    expect_equal(ms_scale_flux_by_area(ms_undo_scale_flux_by_area(w1)),
                     w1)
})

