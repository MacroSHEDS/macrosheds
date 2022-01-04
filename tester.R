library(tidyverse)
library(feather)
library(macrosheds)

#### download_ms_site_data()
site_data <- macrosheds::ms_download_site_data()

#### download_ms_core_data()
macrosheds::ms_download_core_data(macrosheds_root = 'data/ms_test/',
                                  domains = c('hbef', 'hjandrews'),
                                  quiet = F
                                  # networks = 'czo'
                                  )

#### load_product()
all_chem <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                     prodname = 'discharge',
                                     domains = c('hbef'),
                                     # site_codes = c('w1'),
                                     # filter_vars = c('PO4_P', 'NO3_N', 'DOC', 'temp'),
                                     warn = F) %>%
    pull(site_code) %>%
    unique()
    

unique(all_chem$site_code)
unique(all_chem$var)

all_chem %>%
    filter(var == 'GN_DOC') %>%
    ggplot(aes(datetime, val, col = site_code)) +
    geom_point()

#### read_combine_shapefiles()

ws_all <- ms_load_spatial_products('data/ms_test', 'ws_boundary')
mapview::mapview(ws_all)

ws_hbef <- ms_load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    site_codes = c('w1', 'w2', 'w3'))
mapview::mapview(ws_sites)

ws_domains <- ms_load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    domains = c('hbef', 'hjandrews'))
mapview::mapview(ws_domains)

ws_domains_sites <- ms_load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    domains = c('hbef', 'hjandrews'),
                                    site_codes = c('ARIK', 'COMO'))
mapview::mapview(ws_domains_sites)

ws_networks <- ms_load_spatial_products(macrosheds_root = 'data/ms_test', 
                                        spatial_product = 'ws_boundary', 
                                        networks = c('usfs'))
mapview::mapview(ws_networks)

ws_networks_doms <- ms_load_spatial_products(macrosheds_root = 'data/ms_test', 
                                        spatial_product = 'ws_boundary', 
                                        networks = c('usfs'),
                                        domains = 'hjandrews')
mapview::mapview(ws_networks_doms)

#### ms_conversions()

# Check error catch for single input with variables that are not convertible (e.g pH)
d <- feather::read_feather('data/ms_test/hbef/stream_chemistry__ms006/w6.feather') 
new_d <- ms_conversions(d = d, 
                        convert_units_from = 'mg/l',
                        convert_units_to = 'ug/l')

# Check convert molecules
d <- feather::read_feather('data/ms_test/hbef/stream_chemistry__ms006/w6.feather') %>%
    filter(var %in% c('GN_NO3_N', 'GN_SO4_S', 'GN_Mg', 'GN_Na'))
d_new <- d %>%
    ms_conversions(.,
                   convert_units_from = 'mg/l',
                   convert_units_to = 'mg/l',
                   convert_molecules = c('NO3_N', 'SO4_S'))

# moles
d_new <- d %>%
    ms_conversions(.,
                   convert_units_from = 'mg/l',
                   convert_units_to = 'mmol/l')

d <- load_product(macrosheds_root = 'data/ms_test/', 
                  prodname = 'stream_chemistry',
                  sort_result = FALSE,
                  filter_vars = c('NO3_N', 'Na', 'Mg', 'SO4_S'))

converted_data <- ms_conversions(d = d,
                                 convert_units_from = c('NO3_N' = 'mg/l', 
                                                        'Na' = 'mg/l', 
                                                        'Mg' = 'mg/l', 
                                                        'SO4_S' = 'mg/l'),
                                 convert_units_to = c('NO3_N' = 'meq/l',
                                                      'Na' = 'ug/l',
                                                      'Mg' = 'umol/l',
                                                      'SO4_S' = 'g/l'))

#### undo_scale_flux_by_area()
hbef_flux <- macrosheds::load_product(macrosheds_root = 'data/ms_test/', 
                                     prodname = 'stream_flux_inst_scaled',
                                     domains = c('hbef'),
                                     filter_vars = c('NO3_N'))

unscaled_flux <- macrosheds::undo_scale_flux_by_area(hbef_flux)

#### drop_var_prefix()
droped_prefix <- hbef_flux %>%
    mutate(var = macrosheds::drop_var_prefix(var))

#### extract_var_prefix()
prefix <- hbef_flux %>%
    mutate(sampling = macrosheds::extract_var_prefix(var))

#### synchronize_timestep
d <- read_feather('data/ms_test/hbef/stream_chemistry__ms006/w6.feather')
d <- read_feather('data/ms_test/hbef/precipitation__ms900/w1.feather')
d <- d %>%
    filter(!is.na(val)) %>%
    filter(ms_interp == 0)

d_look <- ms_synchronize_timestep(d, 
                                  desired_interval = '1 day',
                                  impute_limit = 10)

d_look <- ms_synchronize_timestep(d, 
                                 desired_interval = '1 year', 
                                 impute_limit = NA,
                                 summary_fun = 'sum')

d_look %>%
    filter(var == 'IN_precipitation') %>%
    ggplot(aes(datetime, val)) + 
    geom_line()


d %>%
    filter(var == 'GN_NO3_N') %>%
    ggplot(aes(datetime, val)) + 
    geom_line()

# ms_run_egret
site_chem <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                        prodname = 'stream_chemistry',
                                        site_codes = c('MARTINELLI'),
                                        filter_vars = c('NO3_N'),
                                        warn = F) %>%
    filter(ms_interp == 0)
site_q <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                      prodname = 'discharge',
                                      site_codes = c('MARTINELLI'),
                                      warn = F)

egret_results <- macrosheds::ms_run_egret(stream_chemistry = site_chem,
                                          discharge = site_q,
                                          prep_data = TRUE,
                                          kalman = F,
                                          quiet = F)


EGRET::plotDiffContours(egret_results, year0=1990, year1=2017)
