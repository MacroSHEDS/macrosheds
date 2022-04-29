library(macrosheds)
library(testthat)


test_that('output watershed area is correct when specs supplied and confirm = FALSE', {
    
    out <- ms_delineate_watershed(
        lat = 44.21013,
        long = -122.2571,
        crs = 4326,
        write_dir = '/tmp/ws_test2',
        write_name = 'example_site',
        spec_buffer_radius_m = 1000,
        spec_snap_distance_m = 150,
        spec_snap_method = 'standard',
        spec_dem_resolution = 10,
        spec_flat_increment = NULL,
        spec_breach_method = 'basic',
        spec_burn_streams = FALSE,
        confirm = FALSE,
        verbose = FALSE
    )
    
    expect_equal(out$watershed_area_ha, 6219.831)
})

test_that('output watershed area is correct when specs supplied and confirm = TRUE (also burning streams)', {
    
    f <- tempfile()
    # ans <- paste(lines, collapse = "\n")
    write("\n", f)
    
    out <- ms_delineate_watershed(
        lat = 44.21013,
        long = -122.2571,
        crs = 4326,
        write_dir = '/tmp/ws_test2',
        write_name = 'example_site',
        spec_buffer_radius_m = 1000,
        spec_snap_distance_m = 150,
        spec_snap_method = 'standard',
        spec_dem_resolution = 10,
        spec_flat_increment = 0.01,
        spec_breach_method = 'basic',
        spec_burn_streams = TRUE,
        confirm = FALSE,
        verbose = FALSE
    )
    
    expect_equal(out$watershed_area_ha, 6219.831)
})

#some specs supplied
#interactive
#confirm = FALSE (legal)
#confirm = FALSE (illegal)
#file written
#abort