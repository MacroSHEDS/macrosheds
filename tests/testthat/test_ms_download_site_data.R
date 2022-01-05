library(macrosheds)
library(testthat)

test_that('site_data file is downloaded', {
    expect_s3_class(macrosheds::ms_download_site_data(),
              class = 'data.frame')
})
