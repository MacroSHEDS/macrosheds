library(tidyverse)
library(feather)
library(macrosheds)

#### download_ms_site_data()
site_data <- macrosheds::download_ms_site_data()

#### download_ms_core_data()
macrosheds::download_ms_core_data(macrosheds_root = 'data/ms_test/',
                                  domains = c('hbef', 'hjandrews'),
                                  # networks = 'czo'
                                  )

#### load_product()
all_chem <- macrosheds::load_product(macrosheds_root = 'data/ms_test/', 
                                     prodname = 'stream_chemistry',
                                     domains = c('hbef', 'hjandrews'),
                                     filter_vars = c('PO4_P', 'NO3_N', 'DOC'))

all_chem %>%
    filter(var == 'GN_DOC') %>%
    ggplot(aes(datetime, val, col = site_code)) +
    geom_point()

#### read_combine_shapefiles()

ws_all <- load_spatial_products('data/ms_test', 'ws_boundary')
mapview::mapview(ws_all)

ws_hbef <- load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    site_codes = c('w1', 'w2', 'w3'))
mapview::mapview(ws_sites)

ws_domains <- load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    domains = c('hbef', 'hjandrews'))
mapview::mapview(ws_domains)

ws_domains_sites <- load_spatial_products(macrosheds_root = 'data/ms_test', 
                                    spatial_product = 'ws_boundary', 
                                    domains = c('hbef', 'hjandrews'),
                                    site_codes = c('ARIK', 'COMO'))
mapview::mapview(ws_domains_sites)

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

