#WRTDS DeBugging ShortCut argument defs
run_output <- adapt_ms_egret(chem_df = ocon_chem, q_df = ocon_q, ws_size = ocon_ws_area_ha)
# EGRET::plotConcHist(egret_results)
# EGRET::plotConcTime(egret_results)
# EGRET::plotFluxQ(egret_results)
# EGRET::plotConcPred(egret_results)

# re-run defnitions cheatsheet

# mas adapt egret
chem_df = chem_df
q_df = q_df
ws_size = area
lat = lat
long = long

## pre egret
  ms_chem <- chem_df %>%
    mutate(site_code = 'none',
           var = 'IS_NO3',
           ms_status = 0,
           ms_interp = 0) %>%
      rename(val = con,
             datetime = all_of(datecol))

    ms_q <- q_df %>%
      mutate(site_code = 'none',
             var = 'IS_discharge',
             ms_status = 0,
             ms_interp = 0) %>%
      rename(val = q_lps,
             datetime = all_of(datecol))

site_data <- tibble(site_code = 'none',
  ws_area_ha = ws_size,
  latitude = lat,
  longitude = long,
  site_type = 'stream_gauge')

stream_chemistry = ms_chem
discharge = ms_q
prep_data = TRUE
site_data = site_data
kalman = FALSE
run_egret = TRUE
quiet = FALSE

minNumObs = 100
minNumUncen = 50
gap_period = 730

verbose = TRUE

windowY = 7
windowQ = 2
windowS = 0.5
edgeAdjust = TRUE
verbose = TRUE
run.parallel = FALSE

surfaces
# surfaceStart=NA
# surfaceEnd=NA
# localSample=NA

