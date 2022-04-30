library(macrosheds)
library(testthat)

#### data for test
temp_dir <- 'data/test_precip_function'
unlink(temp_dir, recursive = TRUE)
dir.create(temp_dir, recursive = TRUE)


coords <- c(-105.64534, 40.03821, -105.58852, 40.05955)
class(coords) <- 'bbox'
ws_boundary <- coords %>%
    sf::st_as_sfc(., crs = 4326) %>%
    sf::st_as_sf() %>%
    mutate(site_code = 'test') %>%
    rename(geometry = x) %>%
    sf::st_set_crs(., 4326)

precip_gauge <- tibble(site_code = c('C1', 'D1', 'saddle'),
                  lat = c(40.0361669465, 40.059693, 40.05492794),
                  long = c(-105.543515181, -105.617012, -105.5893007)) %>%
    sf::st_as_sf(coords = c('long', 'lat'), crs = 4326)

precip <- tibble(C1 = runif(100, min = 0, max = 25),
                 D1 = runif(100, min = 0, max = 50),
                 saddle = runif(100, min = 0, max = 10),
                 datetime = seq.POSIXt(as.POSIXct('2000-01-01', format = '%Y-%m-%d', tz = 'UTC'), 
                                     as.POSIXct('2000-04-09', format = '%Y-%m-%d', tz = 'UTC'), by = 'day')) %>%
    tidyr::pivot_longer(cols = c('D1', 'C1', 'saddle'), names_to = 'site_code',
                 values_to = 'val') %>%
    mutate(var = 'IS_precipitation',
           ms_status = 0,
           ms_interp = 0,
           val_err = 0
           ) %>%
    arrange(site_code, datetime) 

pchem <- tibble(C1 = runif(100, min = 0, max = 25),
                 D1 = runif(100, min = 0, max = 50),
                 saddle = runif(100, min = 0, max = 10),
                datetime = seq.POSIXt(as.POSIXct('2000-01-01', format = '%Y-%m-%d', tz = 'UTC'), 
                                      as.POSIXct('2000-04-09', format = '%Y-%m-%d', tz = 'UTC'), by = 'day')) %>%
    tidyr::pivot_longer(cols = c('D1', 'C1', 'saddle'), names_to = 'site_code',
                 values_to = 'val') %>%
    mutate(var = 'GS_NO3_N',
           ms_status = 0,
           ms_interp = 0,
           val_err = 0) %>%
    arrange(site_code, datetime) 


#### NEED TO SOLVE NO VAL_ERR COLUMN
test_that('precipitation is interpolated without error', {
    
    ms_calc_watershed_precip(precip = precip,
                     ws_boundary = ws_boundary,
                     precip_gauge = precip_gauge,
                     out_path = temp_dir,
                     parallel = T,
                     verbose = T)
    
    fin <- list.files(paste0(temp_dir, '/precipitation__ms900'), full.names = TRUE)
    fin_prod <- feather::read_feather(fin)
    
    expect_s3_class(fin_prod,
                    c('tbl_df', 'tbl', 'data.frame'))
    
    expect_equal(names(fin_prod), 
                 c('datetime', 'site_code', 'var', 'val', 'ms_status', 'ms_interp', 'val_err'))
    
})

# options(error=recover)
test_that('pchem is interpolated without error', {

    ms_calc_watershed_precip(pchem = pchem,
                     ws_boundary = ws_boundary,
                     precip_gauge = precip_gauge,
                     out_path = temp_dir,
                     parallel = T,
                     verbose = T)

    fin <- list.files(paste0(temp_dir, '/precip_chemistry__ms901'), full.names = TRUE)
    fin_prod <- feather::read_feather(fin)

    expect_s3_class(fin_prod,
                    c('tbl_df', 'tbl', 'data.frame'))

    expect_equal(names(fin_prod),
                 c('datetime', 'site_code', 'var', 'val', 'ms_status', 'ms_interp', 'val_err'))

})

test_that('both precipitation and pchem are interpolated without error', {
    
    ms_calc_watershed_precip(precip = precip,
                     ws_boundary = ws_boundary,
                     precip_gauge = precip_gauge,
                     pchem = pchem,
                     out_path = temp_dir,
                     parallel = T,
                     verbose = T)
    
    fin <- list.files(paste0(temp_dir, '/precipitation__ms900'), full.names = TRUE)
    fin_prod <- feather::read_feather(fin)
    
    expect_s3_class(fin_prod,
                    c('tbl_df', 'tbl', 'data.frame'))
    
    expect_equal(names(fin_prod), 
                 c('datetime', 'site_code', 'var', 'val', 'ms_status', 'ms_interp', 'val_err'))
    
})

dir.create(file.path(temp_dir, 'precip_'))
dir.create(file.path(temp_dir, 'pchem_'))
dir.create(file.path(temp_dir, 'ws_bound_', 'wb1'), recursive = TRUE)
dir.create(file.path(temp_dir, 'pgauge_', 'wb1'), recursive = TRUE)

write_feather(precip, file.path(temp_dir, 'precip_', 'precip.feather'))
write_feather(pchem, file.path(temp_dir, 'pchem_', 'pchem.feather'))
sf::st_write(ws_boundary, file.path(temp_dir, 'ws_bound_', 'wb1'),
         driver = 'ESRI Shapefile', delete_dsn = TRUE)
sf::st_write(precip_gauge, file.path(temp_dir, 'pgauge_', 'wb1'),
         driver = 'ESRI Shapefile', delete_dsn = TRUE)

# options(error=recover)

test_that('precip, pchem, precip_gauge, and ws_boundary can be passed as paths', {
    
    ms_calc_watershed_precip(precip = file.path(temp_dir, 'precip_'),
                     ws_boundary = file.path(temp_dir, 'ws_bound_'),
                     precip_gauge = file.path(temp_dir, 'pgauge_'),
                     pchem = file.path(temp_dir, 'pchem_'),
                     out_path = temp_dir,
                     parallel = T,
                     verbose = T)
    
    fin <- list.files(paste0(temp_dir, '/precipitation__ms900'), full.names = TRUE)
    fin_prod <- feather::read_feather(fin)
    
    expect_s3_class(fin_prod,
                    c('tbl_df', 'tbl', 'data.frame'))
    
    expect_equal(names(fin_prod), 
                 c('datetime', 'site_code', 'var', 'val', 'ms_status', 'ms_interp', 'val_err'))
})


fake_ws <- tibble(D1 = c(-102, 85))
fake_pg <- tibble(this_is_a_watershed = c(22,44,55,66,22,11))
test_that('error is throne if non-spatial tables are provided to ws_boundary or precip_gauge', {
    
    expect_error(ms_calc_watershed_precip(precip = precip,
                                  ws_boundary = fake_ws,
                                  precip_gauge = precip_gauge,
                                  out_path = temp_dir,
                                  parallel = T,
                                  verbose = T),
                 'ws_boundary file must be an sf object.')
    
    expect_error(ms_calc_watershed_precip(precip = precip,
                                  ws_boundary = ws_boundary,
                                  precip_gauge = fake_pg,
                                  out_path = temp_dir,
                                  parallel = T,
                                  verbose = T),
                 'precip_gauge file must be an sf object.')
})

precip_fake <- tibble(d1 = c(0,1,2),
                      c1 = c(0,0,0))
pchem_fake <- tibble(d1 = c(0,1,2),
                     c1 = c(0,0,0))

test_that('only macrosheds sheds format is accepted', {
    
    expect_error(ms_calc_watershed_precip(precip = precip_fake,
                                  ws_boundary = ws_boundary,
                                  precip_gauge = precip_gauge,
                                  out_path = temp_dir,
                                  parallel = T,
                                  verbose = T),
                 'precip file must be in macrosheds format with the column names datetime, site_code, val, val_err, and var at minimum')
    
    expect_error(ms_calc_watershed_precip(precip = precip,
                                  pchem = pchem_fake,
                                  ws_boundary = ws_boundary,
                                  precip_gauge = precip_gauge,
                                  out_path = temp_dir,
                                  parallel = T,
                                  verbose = T),
                 'precip_chem file must be in macrosheds format with the column names datetime, site_code, val, val_err, and var at minimum')
    
    })
