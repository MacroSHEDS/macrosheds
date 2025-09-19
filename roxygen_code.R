
setwd('~/git/macrosheds/r_package/')
# setwd('~/git/macrosheds/data_acquisition/')

#regenerate package docs
devtools::document() #preview won't render markdown-style Rd formatting
devtools::build_vignettes()

#reload funcs that have been introduced or modified
devtools::load_all() # just reloads
devtools::install(quick = TRUE) # reloads and reinstalls, without checks

#full install of package (and even beefier install that builds vignettes)
devtools::install()
devtools::install(build_vignettes = TRUE)

#run all tests (optionally log output to file with sink)
sink('tests/results.log')
devtools::test()
sink()

#run individual tests, or groups of tests via regex (though lookarounds not implemented)
devtools::test(filter='ms_download_core_data')
devtools::test(filter='ms_download_ws_attr')
devtools::test(filter='ms_calc_flux')
devtools::test(filter='ms_calc_load')
devtools::test(filter='ms_conversions')
devtools::test(filter='ms_delineate_watershed')
devtools::test(filter='ms_drop_var_prefix')
devtools::test(filter='ms_extract_var_prefix')
devtools::test(filter='ms_generate_attribution')
devtools::test(filter='ms_load_product')
devtools::test(filter='ms_load_sites')
devtools::test(filter='ms_load_spatial_product')
devtools::test(filter='ms_load_variables')
devtools::test(filter='ms_scale_flux_by_area')
devtools::test(filter='ms_undo_scale_flux_by_area')

# devtools::test(filter='ms_calc_watershed_precip')
# devtools::test(filter='ms_identify_usgs_gauges')
# devtools::test(filter='ms_run_egret')
# devtools::test(filter='ms_separate_baseflow')
# devtools::test(filter='ms_synchronize_timestep')

#run all tests (restart session before running)
testthat::test_local(path = 'tests')

#very formally run all tests and example code (necessary for CRAN submission)
devtools::check()
testthat::test_check() #equivalent?
