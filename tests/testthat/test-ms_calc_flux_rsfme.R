library(macrosheds)
library(testthat)

# ms_root = '../data/ms/'
ms_root = '../../data/ms_test/'

chemistry <- ms_load_product(macrosheds_root = ms_root,
                             prodname = 'stream_chemistry',
                             site_codes = c('w1', 'w3', 'w6'),
                             filter_vars = c('NO3_N', 'Cl', 'Na')) %>%
    filter(datetime > "2016-01-01")

q <- ms_load_product(macrosheds_root = ms_root,
                     prodname = 'discharge',
                     site_codes = c('w1', 'w3', 'w6')) %>%
    filter(datetime > "2016-01-01")

test_that("dataframe returned with all input sites, input years, and calculated with all rsfme methods", {

  # input data
  input_sites <- sort(unique(chemistry$site_code))
  input_vars <- sort(unique(ms_drop_var_prefix(chemistry$var)))
  input_methods <- c('average', 'beale', 'composite', 'pw', 'rating')

  # calc flux
  ms_flux <- ms_calc_flux_rsfme(chemistry = chemistry, q = q, q_type = 'discharge', method = 'rsfme')

  # output
  output_sites <- sort(unique(ms_flux$site_code))
  output_vars <- sort(unique(ms_flux$var))
  output_methods <- sort(unique(ms_flux$method))

  # check all sites are present in output "sites" column
  expect_identical(output_sites, input_sites)
  # check all vars are present in output "vars" column
  expect_identical(output_vars, input_vars)
  # check all methods are present in output "methods" column
  expect_identical(output_methods, input_methods)
})
