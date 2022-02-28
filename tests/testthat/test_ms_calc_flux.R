library(macrosheds)
library(lubridate)
library(testthat)

test_no_data <- tibble(datetime = seq.Date(ymd('2000-01-01'), ymd('2001-01-01'),
                                           by = '1 day'),
                       site_code = 'w1',
                       var = 'GN_P',
                       val = 1,
                       ms_status = 0,
                       ms_interp = 0,
                       val_err = 0.01)
test_no_data <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                             as.POSIXct('2001-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                             by = '1 day'),
                       site_code = 'w1',
                       var = 'GN_P',
                       val = 1,
                       ms_status = 0,
                       ms_interp = 0,
                       val_err = 0.01)

discharge <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          as.POSIXct('2001-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                     by = '1 day'),
                 site_code = 'w1',
                 var = 'GN_discharge',
                 val = 1,
                 ms_status = 0,
                 ms_interp = 0,
                 val_err = 0.01)

rando_chem <- rep(c(1,2,5,7,3), 73)
stream_chem <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          as.POSIXct('2000-12-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                            by = '1 day'),
                        site_code = 'w1',
                        var = 'GN_Ca',
                        val = rando_chem,
                        ms_status = 0,
                        ms_interp = 0,
                        val_err = 0.01)

test_that('tibble is returned with correct columns', {
    expect_s3_class(ms_calc_flux(chemistry = stream_chem, q = discharge, q_type = 'discharge'),
                    'data.frame')
    

})

stream_chem_date <- tibble(datetime = seq.Date(ymd('2000-01-01'), 
                                          ymd('2000-12-30'), by = '1 day'),
                      site_code = 'w1',
                      var = 'GN_Ca',
                      val = rando_chem,
                      ms_status = 0,
                      ms_interp = 0,
                      val_err = 0.01)

test_that('function can handle data formats as well as POSIXct', {
    expect_s3_class(ms_calc_flux(chemistry = stream_chem_date, q = discharge, q_type = 'discharge'),
                    'data.frame')
    
    
})

stream_chem_fake <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                            as.POSIXct('2000-12-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                            by = '1 day'),
                      site = 'w1',
                      var = 'GN_Ca',
                      val = 3,
                      ms_status = 0,
                      ms_interp = 0,
                      val_err = 0.01)

discharge_fake <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          as.POSIXct('2001-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          by = '1 day'),
                    site_code = 'w1',
                    varible = 'GN_discharge',
                    val = 1,
                    ms_status = 0,
                    ms_interp = 0,
                    val_err = 0.01)

test_that('Only macrosheds format is accepted', {
    expect_error(ms_calc_flux(chemistry = stream_chem_fake, q = discharge, q_type = 'discharge',
                                      verbose = F))
    
    expect_error(ms_calc_flux(chemistry = stream_chem, q = discharge_fake, q_type = 'discharge',
                                   verbose = F))
    
    
})

discharge_2 <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          as.POSIXct('2001-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                          by = '1 day'),
                    site_code = 'w2',
                    var = 'GN_discharge',
                    val = 1,
                    ms_status = 0,
                    ms_interp = 0,
                    val_err = 0.01)
discharge_2 <- rbind(discharge, discharge_2)

stream_chem_2 <- tibble(datetime = seq.POSIXt(as.POSIXct('2000-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                            as.POSIXct('2000-12-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
                                            by = '1 day'),
                      site_code = 'w2',
                      var = 'GN_Ca',
                      val = rando_chem,
                      ms_status = 0,
                      ms_interp = 0,
                      val_err = 0.01)
stream_chem_2 <- rbind(stream_chem, stream_chem_2)

test_that('All sites are returned', {
    output <- ms_calc_flux(chemistry = stream_chem_2, q = discharge_2, q_type = 'discharge',
                                verbose = F)
    
    expect_equal(unique(output$site_code), c('w1', 'w2'))
    
})
