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

test_that('quiet suppresses outputs', {

    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')
    expect_invisible(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                       domains = c('walker_branch'),
                                                       quiet = TRUE))

    vroot <- paste0('v', substr(list.files(temp_root), 2, 999))
    expect_gte(length(list.files(file.path(temp_root, vroot, 'walker_branch'))), 1)
})

test_that('v1 and v2 are properly retrieved', {

    if(skip_for_now) skip('skipping (switch skip_for_now to FALSE to turn this test on)')
    macrosheds::ms_download_core_data(
        macrosheds_root = temp_root,
        domains = c('walker_branch'),
        version = 1
    )
    macrosheds::ms_download_core_data(
        macrosheds_root = temp_root,
        domains = c('walker_branch'),
        version = 2
    )

    d1 <- macrosheds::ms_load_product(temp_root, 'stream_chemistry', 1, warn = FALSE)
    expect_contains(colnames(d1), 'grab_sample')
    d2 <- macrosheds::ms_load_product(temp_root, 'stream_flux_inst_scaled', 2, warn = FALSE)
    expect_equal(d2$val[1], 8.612889e-05)

    expect_setequal(list.files(file.path(temp_root, 'v2', 'walker_branch')),
                 c('discharge', 'documentation', 'precip_gauge_locations', 'precipitation',
                   'stream_chemistry', 'stream_flux_inst_scaled', 'stream_gauge_locations',
                   'stream_load_scaled_annual', 'ws_boundary'))
})

test_that('data retrieval warnings and errors are issued properly', {
    expect_message(
        expect_message(
            macrosheds::ms_download_core_data(
                macrosheds_root = temp_root,
                domains = c('walker_branch'),
                version = 2
            ),
            'Nothing to do'
        ),
        'Data for these domains already present'
    )

    expect_error(
        macrosheds::ms_download_core_data(
            macrosheds_root = temp_root,
            domains = c('walker_branch'),
            version = 9999
        ),
        'Unknown `version`'
    )

    expect_message(
        expect_message(
            expect_message(
                macrosheds::ms_download_core_data(
                    macrosheds_root = temp_root,
                    domains = c('walker_branch'),
                    version = '1',
                    skip_existing = FALSE
                ),
                'Downloading domain'
            ),
            'successfully downloaded'
        ),
        'All downloads'
    )
})
