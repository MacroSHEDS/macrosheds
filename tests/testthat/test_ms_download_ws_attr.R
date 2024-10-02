library(macrosheds)
library(testthat)
library(feather)

temp_root <- tempdir()
temp_root <- file.path(temp_root, 'ms_test')

skip_for_now <- FALSE
options(timeout = 6000)

# test watershed summary download
test_that("a watershed summary file is download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    expect_invisible(macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                                     dataset = c('summaries')))

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_true(file.exists(file.path(temp_root, vroot, 'watershed_summaries.feather')))
})

# test watershed time series download (of all files)
test_that("all watershed time series files download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    expect_invisible(macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                                     dataset = c('time series'),
                                                     omit_climate_data = TRUE))

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gt(nrow(read_feather(file.path(temp_root, vroot, 'spatial_timeseries_landcover.feather'))), 100)

    ts_vars <- macrosheds::file_ids_for_r_package2 %>%
        filter(grepl('timeseries', ut)) %>%
        filter(!grepl('climate', ut)) %>%
        pull(ut) %>%
        paste(collapse = "|")

    expect_match(list.files(file.path(temp_root, vroot)), ts_vars, all = FALSE)
    expect_no_match(list.files(file.path(temp_root, vroot)), 'climate')
    expect_equal(length(list.files(file.path(temp_root, vroot), pattern = 'spatial_timeseries')), 5)
})

test_that("CAMELS datasets download successfully", {
    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')

    macrosheds::ms_download_ws_attr(macrosheds_root = temp_root,
                                    dataset = c('CAMELS summaries'))

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gt(nrow(read_feather(file.path(temp_root, vroot, 'watershed_summaries_CAMELS.feather'))), 100)
})
