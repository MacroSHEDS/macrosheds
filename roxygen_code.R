
setwd('~/git/macrosheds/r_package/')

#address any errors here before building.
devtools::document('.') #preview won't render markdown-style Rd formatting
# devtools::check('.') #this runs all example code, so we can save it for CRAN submission
devtools::install('.', quick = T)
devtools::install('.', build_vignettes = T)
devtools::install('.')
devtools::load_all('.')
# devtools::install('.', quick = TRUE, build = FALSE) #if you just made a small change and want to reinstall minimally
devtools::test()
# devtools::test(filter='ms_calc_flux') #include only some tests. can't use lookarounds, so not that useful for excluding
# devtools::test(filter='ms_calc_watershed_precip')
# devtools::test(filter='ms_conversions')
# devtools::test(filter='ms_delineate_watershed')
# devtools::test(filter='ms_download_core_data')
# devtools::test(filter='ms_download_site_data')
# devtools::test(filter='ms_run_egret')
# devtools::test(filter='ms_scale_flux_by_area')
# devtools::test(filter='ms_synchronize_timestep')
# devtools::test(filter='ms_download_ws_attr')
devtools::test(filter='ms_generate_attribution')
devtools::test(filter='ms_load_product')
devtools::test(filter='ms_download_ws_attr')
