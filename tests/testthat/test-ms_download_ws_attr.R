library(macrosheds)
library(testthat)
library(feather)

temp_root <- tempdir()
skip_for_now <- FALSE
options(timeout = 1200)

# test watershed summary download
test_that("a watershed summary file is download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    # does function return invisible()
    expect_invisible(macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                                   dataset = c('summaries')))
    # does function return expected filename
    expect_match(list.files(paste0(temp_root)), 'watershed_summaries.feather')

    # is there only one download
    expect_equal(length(list.files(temp_root)), 1)
})

# test watershed time series download (of all files)
test_that("all watershed time series files download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    # does function return invisible()
    expect_invisible(macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                                     dataset = c('time series'),
                                                     omit_climate_data = TRUE))
    
    expect_gt(nrow(read_feather(file.path(temp_root, 'spatial_timeseries_landcover.feather'))), 100)

    ts_vars <- macrosheds::file_ids_for_r_package2 %>%
      filter(grepl('timeseries', ut)) %>%
      filter(!grepl('climate', ut)) %>%
      pull(ut) %>%
      paste(collapse = "|")

    # does function return expected filenames
    expect_match(list.files(paste0(temp_root)), ts_vars, all = FALSE)
    expect_no_match(list.files(paste0(temp_root)), 'climate')

    # are there six downloads (summary, and all time series EXCEPT climate)
    expect_equal(length(list.files(temp_root)), 6)
})

# test watershed time series download (of all files)
test_that("all watershed time series files download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    # does function return invisible()
    expect_invisible(macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                                   dataset = c('time series')))

    ts_vars <- macrosheds::file_ids_for_r_package2 %>%
      filter(grepl('timeseries', ut)) %>%
      pull(ut) %>%
      paste(collapse = "|")

    # does function return expected filename
    expect_match(list.files(paste0(temp_root)), ts_vars, all = FALSE)

    # are there seven downloads (all files)
    expect_gte(length(list.files(temp_root)), 7)
})

test_that("CAMELS datasets download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    # does function return invisible()
    macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                    dataset = c('CAMELS summaries'))
    
    expect_gt(nrow(read_feather(file.path(temp_root, 'watershed_summaries_CAMELS.feather'))), 100)
})
