library(macrosheds)
library(testthat)

datetime_string <- c(lubridate::ymd('2000-01-01', '2000-01-02', '2000-01-03',
                         '2000-01-04', '2000-01-05', '2000-01-06'))

no3 <- tibble(datetime = datetime_string,
              site_code = 'w1', 
              var = 'GN_NO3', 
              val = c(seq(0.2, 1.2, by = 0.2)),
              ms_status = 0,
              ms_interp = 0,
              val_err = 0)

no3_n <- tibble(datetime = datetime_string,
                site_code = 'w1', 
                var = 'GN_NO3_N', 
                val = c(seq(0.2, 1.2, by = 0.2)),
                ms_status = 0,
                ms_interp = 0,
                val_err = 0)

cl <- tibble(datetime = datetime_string,
             site_code = 'w1', 
             var = 'GN_Cl', 
             val = c(seq(0.2, 1.2, by = 0.2)),
             ms_status = 0,
             ms_interp = 0,
             val_err = 0)

ca <- tibble(datetime = datetime_string,
             site_code = 'w1', 
             var = 'GN_Ca', 
             val = c(seq(0.2, 1.2, by = 0.2)),
             ms_status = 0,
             ms_interp = 0,
             val_err = 0)

test_that('single input thows error with varibles of diffrent units', {
    
    fake_var <- tibble(datetime = datetime_string,
                       site_code = 'w1', 
                       var = c('GN_nonon'), 
                       val = c(seq(0.2, 1.2, by = 0.2)),
                       ms_status = 0,
                       ms_interp = 0,
                       val_err = 0)
    
    expect_error(macrosheds::ms_conversions(d = fake_var,
                                            convert_units_from = 'ug/l',
                                            convert_units_to = 'mg/l',
                                            macrosheds_root = 'data/'))
})

test_that('non macrosheds variables supplied to the funciton throw error', {
    
    ph_ca <- tibble(datetime = datetime_string,
                    site_code = 'w1', 
                    var = c('GN_Ca', 'GN_Ca', 'GN_Ca', 
                            'GN_pH', 'GN_pH', 'GN_pH'), 
                    val = c(seq(0.2, 1.2, by = 0.2)),
                    ms_status = 0,
                    ms_interp = 0,
                    val_err = 0)
    
    expect_error(macrosheds::ms_conversions(d = ph_ca,
                                            convert_units_from = 'ug/l',
                                            convert_units_to = 'mg/l',
                                            macrosheds_root = 'data/'))
})

test_that('when there are different variables and not all specified, throw error', {
    
    multi_var <- tibble(datetime = datetime_string,
                        site_code = 'w1', 
                        var = c('GN_Ca', 'GN_Ca', 'GN_Ca', 
                                'GN_Cl', 'GN_Na', 'GN_Li'), 
                        val = c(seq(0.2, 1.2, by = 0.2)),
                        ms_status = 0,
                        ms_interp = 0,
                        val_err = 0)
    
    expect_error(macrosheds::ms_conversions(d = multi_var,
                                            convert_units_from = c('Ca' = 'ug/l',
                                                                   'Na' = 'ug/l'),
                                            convert_units_to = c('Ca' = 'mg/l',
                                                                 'Na' = 'mg/l'),
                                            macrosheds_root = 'data/'))
})

test_that('when names of variables are diffrent, throw error', {
    
    multi_var <- tibble(datetime = datetime_string,
                        site_code = 'w1', 
                        var = c('GN_Ca', 'GN_Ca', 'GN_Ca', 
                                'GN_Cl', 'GN_Cl', 'GN_Cl'), 
                        val = c(seq(0.2, 1.2, by = 0.2)),
                        ms_status = 0,
                        ms_interp = 0,
                        val_err = 0)
    
    expect_error(macrosheds::ms_conversions(d = multi_var,
                                            convert_units_from = c('Ca' = 'ug/l',
                                                                   'Cl' = 'ug/l',
                                                                   'Li' = 'ug/l'),
                                            convert_units_to = c('Ca' = 'mg/l',
                                                                 'Na' = 'mg/l',
                                                                 'Cl' = 'mg/l'),
                                            macrosheds_root = 'data/'))
})

test_that('metric conversions are correct', {
    
    ug_results <- macrosheds::ms_conversions(d = cl,
                                             convert_units_from = 'ug/l',
                                             convert_units_to = 'mg/l',
                                             macrosheds_root = 'data/')
    expect_equal(round(ug_results$val, 4), 
                 c(0.0002, 0.0004, 0.0006, 0.0008, 0.0010, 0.0012))
    
    g_results <- macrosheds::ms_conversions(d = cl,
                                            convert_units_from = 'g/l',
                                            convert_units_to = 'mg/l',
                                            macrosheds_root = 'data/')
    expect_equal(g_results$val, 
                 c(200, 400, 600, 800, 1000, 1200))
    
})

test_that('molar conversions are correct', {
    
    moles_results <- macrosheds::ms_conversions(d = cl,
                                                convert_units_from = 'mmol/l',
                                                convert_units_to = 'mg/l',
                                                macrosheds_root = 'data/')
    expect_equal(round(moles_results$val, 2), 
                 c(7.09, 14.18, 21.27, 28.36, 35.45, 42.54))
    
    moles_g_results <- macrosheds::ms_conversions(d = cl,
                                                  convert_units_from = 'mg/l',
                                                  convert_units_to = 'mmol/l',
                                                  macrosheds_root = 'data/')
    expect_equal(round(moles_g_results$val, 5), 
                 c(0.00564, 0.01128, 0.01692, 0.02257, 0.02821, 0.03385))
    
})

test_that('equivalents conversions are correct', {
    
    eq_results <- macrosheds::ms_conversions(d = ca,
                                             convert_units_from = 'eq/l',
                                             convert_units_to = 'g/l',
                                             macrosheds_root = 'data/')
    expect_equal(round(eq_results$val, 2), 
                 c(4.01, 8.02, 12.02, 16.03, 20.04, 24.05))
    
    mg_eq_results <- macrosheds::ms_conversions(d = cl,
                                                convert_units_from = 'g/l',
                                                convert_units_to = 'eq/l',
                                                macrosheds_root = 'data/')
    expect_equal(round(mg_eq_results$val, 4), 
                 c(0.0056, 0.0113, 0.0169, 0.0226, 0.0282, 0.0338))
    
})

test_that('molecular conversions are correct', {
    
    molecular_results <- macrosheds::ms_conversions(d = no3,
                                                    convert_units_from = 'mg/l',
                                                    convert_units_to = 'mg/l',
                                                    convert_molecules = 'NO3',
                                                    macrosheds_root = 'data/')
    expect_equal(round(molecular_results$val, 3), 
                 c(0.045, 0.090, 0.136, 0.181, 0.226, 0.271))
    
    molecular_full_results <- macrosheds::ms_conversions(d = no3_n,
                                                         convert_units_from = 'mg/l',
                                                         convert_units_to = 'mg/l',
                                                         convert_molecules = 'NO3_N',
                                                         macrosheds_root = 'data/')
    expect_equal(round(molecular_full_results$val, 3), 
                 c(0.885, 1.771, 2.656, 3.541, 4.427, 5.312))
    
})

test_that('molecular and molar conversions are correct', {
    
    molecular_results <- macrosheds::ms_conversions(d = no3,
                                                    convert_units_from = 'mmol/l',
                                                    convert_units_to = 'mg/l',
                                                    convert_molecules = 'NO3',
                                                    macrosheds_root = 'data/')
    expect_equal(round(molecular_results$val, 3), 
                 c(2.801, 5.603, 8.404, 11.205, 14.007, 16.808))
    
    molecular_full_results <- macrosheds::ms_conversions(d = no3_n,
                                                         convert_units_from = 'mg/l',
                                                         convert_units_to = 'mmol/l',
                                                         convert_molecules = 'NO3_N',
                                                         macrosheds_root = 'data/')
    expect_equal(round(molecular_full_results$val, 3), 
                 c(0.063, 0.126, 0.190, 0.253, 0.316, 0.379))
    
})
