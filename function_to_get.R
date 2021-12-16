# list of function that we will want to include 
calc_inst_flux (ms_compute_stream_flux)
    d_discharge, d_chemistry, method = c('inst', 'vwc', 'vwf', etc)
ms_compute_precip_flux
ms_linear_interpolate (ms_interpolate)
    d, method = c('linear', 'seadec', 'wrtds', etc)
delineate_watershed_apriori (ms_delineate_watershed)
    a priori (if no spec supplied)
    by spec (if any spec supplied)
    confirm = TRUE
synchronize_timestep
scale_flux_by_area
get_nrcs_soils
