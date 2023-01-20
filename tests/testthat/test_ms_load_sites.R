library(macrosheds)
library(testthat)

test_that('sites load', {
    expect_s3_class(ms_load_sites(), 'data.frame')
})
