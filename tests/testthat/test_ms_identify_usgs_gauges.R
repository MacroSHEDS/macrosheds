library(macrosheds)
library(testthat)

sites <- tibble(site_code = c('MA_AE03', 'MI_KR01'),
                Latitude = c(42.04856, 42.28575),
                Longitude = c(-72.45403, -85.51467))


site_sf <- sites %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) 

test_that('function works with lat long inputs', {
    
    expect_s3_class(ms_identify_usgs_gauges(sites = sites, lat = 'Latitude', long = 'Longitude'),
                    'data.frame')
})


test_that('function works with sf', {
    
    result <- 'usgs_site' %in% names(ms_identify_usgs_gauges(sites = site_sf)) 
    
    expect_true(result)
    
})

test_that('data.frame with usgs_site column is returned', {
    
    expect_error(ms_identify_usgs_gauges(sites = sites, long = 'Longitude'),
                 'Both lat and long must be provided or the argument to sites must be an sf object')
    expect_error(ms_identify_usgs_gauges(sites = sites, lat = 'Longitude'),
                 'Both lat and long must be provided or the argument to sites must be an sf object')
})

site_sf_weird <- sites %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
    sf::st_transform(., crs = 32610)

test_that('function works with non WGS84 sf object', {
    
    expect_equal(nrow(filter(ms_identify_usgs_gauges(sites = site_sf_weird), !is.na(usgs_site))), 1)
    
})

test_that('error is thrown for only one input of lat or long', {
    
    expect_error(ms_identify_usgs_gauges(sites = sites, long = 'Longitude'),
                    'Both lat and long must be provided or a sf objust must be the input to site')
    expect_error(ms_identify_usgs_gauges(sites = sites, lat = 'Longitude'),
                 'Both lat and long must be provided or a sf objust must be the input to site')
    
})
