library(macrosheds)
library(lubridate)
library(testthat)

temp_root <- tempdir()

test_that('a domain is downloaded successfully', {
    
    expect_output(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                   domains = c('hbef')),
                  'hbef successfully downloaded and unziped')
    
    expect_gte(length(list.files(paste0(temp_root, '/hbef'))), 1)
})

test_that('a network is downloaded successfully', {
    
    expect_output(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                    networks = c('czo')),
                  'shale_hills successfully downloaded and unziped')
    
    expect_gte(length(list.files(paste0(temp_root, '/boulder'))), 1)
    expect_gte(length(list.files(paste0(temp_root, '/shale_hills'))), 1)
    expect_gte(length(list.files(paste0(temp_root, '/calhoun'))), 1)
    expect_gte(length(list.files(paste0(temp_root, '/catalina_jemez'))), 1)
    
})

test_that('if warn removes outputs', {
    
    expect_visible(macrosheds::ms_download_core_data(macrosheds_root = temp_root,
                                                    domains = c('walker_branch'),
                                                    quiet = TRUE))
    
    expect_gte(length(list.files(paste0(temp_root, '/walker_branch'))), 1)
    
})
