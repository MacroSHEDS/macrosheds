library(macrosheds)
library(lubridate)
library(testthat)

temp_root <- tempdir()
temp_root <- file.path(temp_root, 'ms_test')
skip_for_now <- FALSE
options(timeout = 6000)

test_that('a domain is downloaded successfully', {

    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')
    expect_output(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                    domains = c('hbef')),
                  'hbef successfully downloaded and unzipped')

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gte(length(list.files(file.path(temp_root, vroot, 'hbef'))), 10)
})

test_that('a network is downloaded successfully', {

    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')
    expect_output(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                    networks = c('czo')),
                  'shale_hills successfully downloaded and unzipped')

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gte(length(list.files(file.path(temp_root, vroot, 'boulder'))), 1)
    expect_gte(length(list.files(file.path(temp_root, vroot, 'shale_hills'))), 1)
    expect_gte(length(list.files(file.path(temp_root, vroot, 'calhoun'))), 1)
    expect_gte(length(list.files(file.path(temp_root, vroot, 'catalina_jemez'))), 1)
})

test_that('if warn removes outputs', {

    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')
    expect_invisible(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                       domains = c('walker_branch'),
                                                       quiet = TRUE))

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gte(length(list.files(file.path(temp_root, vroot, 'walker_branch'))), 1)
})
