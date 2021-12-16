# list of function that we will want to include 
calc_inst_flux (ms_compute_stream_flux)
    d_discharge, d_chemistry, method = c('inst', 'vwc', 'vwf', etc)
ms_compute_precip_flux
ms_linear_interpolate (ms_interpolate)
    d, method = c('linear', 'seadec', 'wrtds', etc)
delineate_watershed_apriori (ms_delineate_watershed)
    implicitly a priori if no spec supplied
    use any supplied specs
    if all specs are given, a confirm = FALSE option will just return the delineated shed. this is equivalent to delineate_by_specification
synchronize_timestep
scale_flux_by_area
get_nrcs_soils
