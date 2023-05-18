library(macrosheds)
library(testthat)

# setwd('./tests/testthat/')
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

ppt <- ms_load_product(macrosheds_root = ms_root,
                     prodname = 'precipitation',
                     site_codes = c('w1', 'w3', 'w6')) %>%
    filter(datetime > "2016-01-01")

test_that("dataframe returned with all input sites, input years, and calculated with all rsfme methods", {

  # input data
  input_sites <- sort(unique(chemistry$site_code))
  input_vars <- sort(unique(ms_drop_var_prefix(chemistry$var)))
  input_methods <- c('average', 'beale', 'composite', 'pw', 'rating')

  # calc flux
  ms_flux <- ms_calc_flux_rsfme(chemistry = chemistry, q = q, method = input_methods, aggregation = 'annual')

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

test_that("dataframe gets and checks validity of Q type from input data", {
  # attempt to use rsfme flux methods on precip data
  ms_flux <- try(ms_calc_flux_rsfme(chemistry = chemistry, q = ppt, method = 'beale', aggregation = c('monthly')))
  # check that this registers as an error
  expect_true(inherits(ms_flux, 'try-error'))
  
  # correct arguments, calculate "simple" precip flux
  ms_flux <- try(ms_calc_flux_rsfme(chemistry = chemistry, q = ppt, method = 'simple'))
  # check that this produces a dataframe
  expect_true(is.data.frame(ms_flux))
})

test_that("dataframe returned with correct input methods and aggregation", {
  # input methods
  input_methods <- c('average', 'composite', 'pw')
  # calc flux (annual)
  ms_flux <- ms_calc_flux_rsfme(chemistry = chemistry, q = q, method = input_methods, aggregation = 'annual')
  # output
  output_methods <- sort(unique(ms_flux$method))
  # check only user input methods are present in output "methods" column
  expect_identical(output_methods, input_methods)
  # check "month" not in column names
  expect_false("month" %in% colnames(ms_flux))
  
  # input methods
  input_methods <- c('rating', 'pw', 'beale')
  # calc flux (monthly)
  ms_flux <- ms_calc_flux_rsfme(chemistry = chemistry, q = q, method = input_methods, aggregation = 'monthly')
  # output
  output_methods <- sort(unique(ms_flux$method))
  # check only user input methods are present in output "methods" column
  expect_setequal(output_methods, input_methods)
  # check "month" is in column names
  expect_true("month" %in% colnames(ms_flux))
  expect_true("month" %in% colnames(ms_flux))
})
