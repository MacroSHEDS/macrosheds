library(macrosheds)
library(testthat)

whitebox::wbt_init(exe_path = '~/git/others_projects/whitebox-tools/target/release/whitebox_tools')

test_that('output watershed area is correct when specs supplied and confirm = FALSE. files written.', {

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

    expect_equal(round(out$watershed_area_ha), 6220)
    expect_true(file.exists(file.path(out$out_path, paste0(out$filename_base, '.shp'))))
    expect_true(file.exists(file.path(out$out_path, paste0(out$filename_base, '.shx'))))
    expect_true(file.exists(file.path(out$out_path, paste0(out$filename_base, '.prj'))))
    expect_true(file.exists(file.path(out$out_path, paste0(out$filename_base, '.dbf'))))
})

test_that('output watershed area is correct when specs supplied and confirm = TRUE (also burning streams)', {

    f <- tempfile()
    write('1', f)

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
        verbose = FALSE,
        responses_from_file = f
    )

    expect_equal(round(out$watershed_area_ha), 6218) #interesting that burn streams changes area slightly
})

test_that('all options can be set interactively by user', {

    f <- tempfile()
    writeLines(c('MDSBURI', '1', '200', '10000', '9', 'L', 'a'), f)

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
        spec_burn_streams = FALSE,
        verbose = FALSE,
        responses_from_file = f
    )

    expect_null(out)
})

test_that('flipping through multiple candidates works', {

    f <- tempfile()
    writeLines(c('', '2'), f)

    out <- ms_delineate_watershed(
        lat = 44.210,
        long = -122.251,
        crs = 4326,
        write_dir = '/tmp/ws_test2',
        write_name = 'example_site',
        spec_buffer_radius_m = 1000,
        spec_dem_resolution = 10,
        spec_flat_increment = 0.01,
        spec_breach_method = 'basic',
        spec_burn_streams = FALSE,
        verbose = FALSE,
        responses_from_file = f
    )

    expect_equal(round(out$watershed_area_ha), 2)
})

test_that('illegal operations raise error', {

    f <- tempfile()
    writeLines(c('MDSBURI', '1', '200', '10000', '9', 'L', 'a'), f)

    expect_error(ms_delineate_watershed(
        lat = 44.21013,
        crs = 4326
    ))
    expect_error(ms_delineate_watershed(
        lat = 44.21013,
        long = -122.2571,
        crs = 4326,
        write_dir = '/tmp/ws_test2',
        verbose = FALSE,
        confirm = FALSE
    ))
    expect_error(ms_delineate_watershed(
        lat = 44.21013,
        long = -122.2571,
        crs = NULL,
        write_dir = '/tmp/ws_test2',
        write_name = 'aa',
        verbose = FALSE,
        confirm = FALSE
    ))
})
