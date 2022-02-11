library(tidyverse)
library(feather)
library(macrosheds)
library(lubridate)

#### download_ms_site_data() ####
site_data <- macrosheds::ms_download_site_data()

#### download_ms_core_data() ####
macrosheds::ms_download_core_data(macrosheds_root = 'data/ms_test/',
                                  domains = c('hjandrews'),
                                  quiet = F
                                  # networks = 'czo'
                                  )

#### load_product() ####
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

#### read_combine_shapefiles() ####

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

#### ms_conversions() ####

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

#### undo_scale_flux_by_area() ####
hbef_flux <- macrosheds::load_product(macrosheds_root = 'data/ms_test/', 
                                     prodname = 'stream_flux_inst_scaled',
                                     domains = c('hbef'),
                                     filter_vars = c('NO3_N'))

unscaled_flux <- macrosheds::undo_scale_flux_by_area(hbef_flux)

#### drop_var_prefix() ####
droped_prefix <- hbef_flux %>%
    mutate(var = macrosheds::drop_var_prefix(var))

#### extract_var_prefix() ####
prefix <- hbef_flux %>%
    mutate(sampling = macrosheds::extract_var_prefix(var))

#### synchronize_timestep ####
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

#### ms_run_egret ####
site_chem <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                        prodname = 'stream_chemistry',
                                        site_codes = c('ALBION'),
                                        filter_vars = c('NO3_N'),
                                        warn = F) %>%
    filter(ms_interp == 0)
site_q <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/', 
                                      prodname = 'discharge',
                                      site_codes = c('ALBION'),
                                      warn = F)

egret_results <- macrosheds::ms_run_egret(stream_chemistry = site_chem,
                                          discharge = site_q,
                                          prep_data = TRUE,
                                          kalman = F,
                                          quiet = F)


EGRET::plotDiffContours(egret_results, year0=1990, year1=2017)

#### ms_precip_interp ####
pchem_path <- list.files('../data_processing/data/czo/boulder/munged/precip_chemistry__3639/', full.names = TRUE)
pchem <- map_dfr(pchem_path, read_feather)
precip_path1 <- list.files('../data_processing/data/czo/boulder/munged/precipitation__2435/', full.names = TRUE)
precip_path2 <- list.files('../data_processing/data/czo/boulder/munged/precipitation__2888/', full.names = TRUE)
precip_path3 <- list.files('../data_processing/data/czo/boulder/munged/precipitation__2889/', full.names = TRUE)
precip_path <- c(precip_path1, precip_path2, precip_path3)
precip_ <- map_dfr(precip_path, read_feather)
wb_path <- list.files('../data_processing/data/czo/boulder/derived/ws_boundary__ms000/', full.names = TRUE)
ws_boundary <- map_dfr(wb_path, sf::st_read)
pgauge_path <- list.files('../data_processing/data/czo/boulder/derived/precip_gauge_locations__ms002/', full.names = TRUE)
precip_gauge <- map_dfr(pgauge_path, sf::st_read)

fake_tib <- tibble()

ms_precip_interp(precip = precip,
                 ws_boundary = ws_boundary,
                 precip_gauge = '../data_processing/data/czo/boulder/derived/precip_gauge_locations__ms002/',
                 pchem = pchem,
                 out_path = 'data/precip_test_',
                 parallel = T,
                 verbose = T)

all_files <- list.files('data/precip_test/precipitation__ms900/', full.names = TRUE)

look <- map_dfr(all_files, read_feather)
look %>%
    ggplot(aes(datetime, val, col = var, shape = site_code)) +
    geom_point()



#### Investigate ####
# Sites 
dist_record <- read_csv('../macrosheds_support/data/disturbance_record.csv') %>%
    filter(site_code %in% c('w1', 'w2', 'w4', 'w5', 'w6', 'GSWS01', 'GSWS02',
                            'GSWS06', 'GSWS07', 'GSWS08', 'GSWS08', 'WS-1', 'WS-2',
                            'WS-3', 'WS-4', 'WS-5', 'WS-6', 'WS-7'))

site_data <- macrosheds::ms_download_site_data() %>%
    pull(site_code)

net_dom <- macrosheds::ms_download_site_data() %>%
    select(site_code, domain, network, site_type)

ph_data <- macrosheds::ms_load_product(macrosheds_root = 'data/ms_test/',
                                       prodname = 'stream_chemistry',
                                       # domains = 'krycklan',
                                       # site_codes = site_data,
                                       # filter_vars = 'pH',
                                       warn = F)

# domains = 'kryklan',
# site_codes = c('w1', 'w2', 'w4', 'w5', 'w6', 'GSWS01', 'GSWS02',
#                'GSWS06', 'GSWS07', 'GSWS08', 'GSWS08', 'WS-1', 'WS-2',
#                'WS-3', 'WS-4', 'WS-5', 'WS-6', 'WS-7',
#                'WB', 'EB'),

annual <- ph_data %>%
    mutate(month = month(datetime),
           year = year(datetime)) %>%
    group_by(year, site_code) %>%
    mutate(mean_val = mean(val, na.rm = T)) %>%
    left_join(net_dom) %>%
    full_join(dist_record) 

annual %>%
    filter(site_type == 'stream_gauge') %>%
    filter(domain %in% c('arctic', 'baltimore', 'fernow', 'hbef', 'hjandrews', 'luquillo', 'niwot', 'walker_branch')) %>%
    ggplot(aes(year, mean_val, col = watershed_type, group = site_code)) +
    geom_line() +
    facet_wrap(~domain) +
    theme(legend.position = 'none')

#### ms_identify_usgs_gauges ####
sites <- read_csv('../macrosheds_support/data/SOTF_Data - All_Data.csv') %>%
    distinct(map_code, .keep_all = TRUE) %>%
    filter(!is.na(Longitude),
           !is.na(Latitude)) 

sites <- sites[1:8,]

sites <- sites %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) 

look <- ms_identify_usgs_gauges(sites = sites)

sites <- read_csv('../macrosheds_support/data/SOTF_Data - All_Data.csv') %>%
    distinct(map_code, .keep_all = TRUE) %>%
    filter(!is.na(Longitude),
           !is.na(Latitude)) 

sites <- sites[1:8,]
look_2 <- ms_identify_usgs_gauges(sites = sites, lat = 'Latitude',
                                long = 'Longitude')




sites <- read_csv('../macrosheds_support/data/SOTF_Data - All_Data.csv') %>%
    distinct(map_code, .keep_all = TRUE) %>%
    filter(!is.na(Longitude),
           !is.na(Latitude)) 

sites <- sites[1:8,]

sites <- sites %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4269) %>%
    sf::st_transform(., crs = 32610)

look_3 <- ms_identify_usgs_gauges(sites = sites)

sites <- tibble(site_code = c('MA_AE03', 'MI_KR01'),
                Latitude = c(42.04856, 42.28575),
                Longitude = c(-72.45403, -85.51467))

ms_identify_usgs_gauges(sites = sites, lat = 'Latitude', long = 'Longitude')


#### ms_calc_inst_flux ####
chemistry <- macrosheds::ms_load_product('data/ms_test/',
                                         'precip_chemistry',
                                         site_codes = c('w1', 'w3', 'w6'),
                                         filter_vars = c('NO3_N', 'Cl', 'Na'),
                                         warn = F)

q <- macrosheds::ms_load_product('data/ms_test/',
                                 'precipitation',
                                 site_codes = c('w1', 'w3', 'w6'),
                                 warn = F)

look <- macrosheds::ms_calc_inst_flux(chemistry, q, q_type = 'precipitation')

flux <- macrosheds::ms_load_product('data/ms_test/',
                                    'precip_flux_inst_scaled',
                                    site_codes = c('w1', 'w3', 'w6'),
                                    filter_vars = c('NO3_N', 'Cl', 'Na'),
                                    warn = F)



chemistry <- macrosheds::ms_load_product('data/ms_test/',
                                         'stream_chemistry',
                                         site_codes = c('w1', 'w3', 'w6'),
                                         filter_vars = c('NO3_N', 'Cl', 'Na'),
                                         warn = F)

q <- macrosheds::ms_load_product('data/ms_test/',
                                 'discharge',
                                 site_codes = c('w1', 'w3', 'w6'),
                                 warn = F)

look <- ms_calc_inst_flux(chemistry, q, q_type = 'discharge')


flux <- macrosheds::ms_load_product('data/ms_test/',
                                         'stream_flux_inst_scaled',
                                         site_codes = c('w1', 'w3', 'w6'),
                                         filter_vars = c('NO3_N', 'Cl', 'Na'),
                                         warn = F)

test=ms_undo_scale_flux_by_area(flux)
