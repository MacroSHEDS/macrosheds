library(macrosheds)
library(lubridate)
library(testthat)

test_no_data <- tibble(date = seq.Date(lubridate::ymd('2000-01-01'), lubridate::ymd('2001-01-01'),
                                       by = '1 day'),
                       site_code = 'w1',
                       var = 'orthophosphate',
                       val = 1,
                       ms_status = 0,
                       ms_interp = 0,
                       val_err = 0.01)

discharge <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                    as.Date('2001-01-01', format = '%Y-%m-%d'),
                                    by = '1 day'),
                    site_code = 'w1',
                    var = 'discharge',
                    val = 1,
                    ms_status = 0,
                    ms_interp = 0,
                    val_err = 0.01)

rando_chem <- rep(c(1,2,5,7,3), 73)
stream_chem <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                      as.Date('2000-12-30', format = '%Y-%m-%d'),
                                      by = '1 day'),
                      site_code = 'w1',
                      var = 'Ca',
                      val = rando_chem,
                      ms_status = 0,
                      ms_interp = 0,
                      val_err = 0.01)

test_that('tibble is returned with correct columns', {
    expect_s3_class(ms_calc_flux(chemistry = stream_chem, q = discharge),
                    'data.frame')
})

stream_chem_date <- tibble(date = seq.Date(lubridate::ymd('2000-01-01'),
                                           lubridate::ymd('2000-12-30'), by = '1 day'),
                           site_code = 'w1',
                           var = 'Ca',
                           val = rando_chem,
                           ms_status = 0,
                           ms_interp = 0,
                           val_err = 0.01)

test_that('function can handle data formats as well as Date', {
    expect_s3_class(ms_calc_flux(chemistry = stream_chem_date, q = discharge),
                    'data.frame')
})

stream_chem_fake <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                           as.Date('2000-12-30', format = '%Y-%m-%d'),
                                           by = '1 day'),
                           site = 'w1',
                           var = 'Ca',
                           val = 3,
                           ms_status = 0,
                           ms_interp = 0,
                           val_err = 0.01)

discharge_fake <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                         as.Date('2001-01-01', format = '%Y-%m-%d'),
                                         by = '1 day'),
                         site_code = 'w1',
                         varible = 'discharge',
                         val = 1,
                         ms_status = 0,
                         ms_interp = 0,
                         val_err = 0.01)

test_that('Only macrosheds format is accepted', {
    expect_error(ms_calc_flux(chemistry = stream_chem_fake, q = discharge,
                              verbose = F))

    expect_error(ms_calc_flux(chemistry = stream_chem, q = discharge_fake,
                              verbose = F))
})

discharge_2 <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                      as.Date('2001-01-01', format = '%Y-%m-%d'),
                                      by = '1 day'),
                      site_code = 'w2',
                      var = 'discharge',
                      val = 1,
                      ms_status = 0,
                      ms_interp = 0,
                      val_err = 0.01)
discharge_2 <- rbind(discharge, discharge_2)

stream_chem_2 <- tibble(date = seq.Date(as.Date('2000-01-01', format = '%Y-%m-%d'),
                                        as.Date('2000-12-30', format = '%Y-%m-%d'),
                                        by = '1 day'),
                        site_code = 'w2',
                        var = 'Ca',
                        val = rando_chem,
                        ms_status = 0,
                        ms_interp = 0,
                        val_err = 0.01)
stream_chem_2 <- rbind(stream_chem, stream_chem_2)

test_that('All sites are returned', {
    output <- ms_calc_flux(chemistry = stream_chem_2, q = discharge_2)

    expect_equal(unique(output$site_code), c('w1', 'w2'))
})
