library(macrosheds)
library(testthat)
library(feather)

# setwd('./tests/testthat/')
#ms_root <- '../../data/ms_test' #superfluous files in the data/ directory cause problems for build.
# ms_root <- '~/ssd2/ms_test' #so use a directory that works for your machine
ms_root <- tempdir()
ms_root <- file.path(ms_root, 'ms_test')

flux_fp <- 'data/ms_test/ms_flux_annual'

test_sites <- c('w1', 'w3', 'w6')
test_vars <- c('NO3_N', 'Cl', 'Na')
test_methods <- c('average', 'beale', 'composite', 'pw', 'rating')

chemistry <- ms_load_product(macrosheds_root = ms_root,
                             prodname = 'stream_chemistry',
                             site_codes = test_sites,
                             filter_vars = test_vars) %>%
    filter(date > as.Date('2016-01-01'))

q <- ms_load_product(macrosheds_root = ms_root,
                     prodname = 'discharge',
                     site_codes = test_sites) %>%
    filter(date > as.Date('2016-01-01'))

ppt <- ms_load_product(macrosheds_root = ms_root,
                       prodname = 'precipitation',
                       site_codes = test_sites) %>%
    filter(date > as.Date('2016-01-01'))

ms_load <- ms_calc_load(chemistry = chemistry,
                        q = q,
                        method = test_methods)
                        # aggregation = 'annual')

# flux_gubbins <- feather::read_feather(file.path(flux_fp, 'baltimore/stream_flux/BARN.feather')) %>%
#     rbind(feather::read_feather(file.path(flux_fp, 'hjandrews/stream_flux/GSMACK.feather'))) %>%
#     rbind(feather::read_feather(file.path(flux_fp, 'hbef/stream_flux/w6.feather'))) %>%
#     mutate(var = ms_drop_var_prefix(var)) %>%
#     filter(method != 'wrtds')

test_that("dataframe returned with all input sites, input years, and calculated with all rsfme methods", {

    output_sites <- sort(unique(ms_load$load$site_code))
    output_vars <- sort(unique(ms_load$load$var))
    output_methods <- sort(unique(ms_load$load$method))

    expect_setequal(output_sites, test_sites)
    expect_setequal(output_vars, test_vars)
    expect_setequal(output_methods, test_methods)
})

test_that("dataframe gets and checks validity of Q type from input data", {

    expect_error({
        ms_calc_load(chemistry = chemistry, q = ppt, method = 'beale')
    }, '`q$var` must be "discharge"',
    fixed = TRUE)
})

test_that("dataframe returned with correct input methods and aggregation, multiple combinations, use-case testing", {

    # NORMAL use-cases, a few methods across annual and monthly
    # input methods
    test_methods <- c('average', 'composite', 'pw')
    # calc flux (annual)
    ms_load <- ms_calc_load(chemistry = chemistry, q = q, method = test_methods)
    # output
    output_methods <- sort(unique(ms_load$load$method))
    # check only user input methods are present in output "methods" column
    expect_identical(output_methods, test_methods)
    # check "month" not in column names
    expect_false("month" %in% colnames(ms_load))

    # # input methods
    # test_methods <- c('rating', 'pw', 'beale')
    # # calc flux (monthly)
    # ms_load <- ms_calc_load(chemistry = chemistry, q = q, method = test_methods, aggregation = 'monthly')
    # # output
    # output_methods <- sort(unique(ms_load$method))
    # # check only user input methods are present in output "methods" column
    # expect_setequal(output_methods, test_methods)
    # # check "month" is in column names
    # expect_true("month" %in% colnames(ms_load))

    # # trying to use off-format dataset
    # ms_load <- try({ms_calc_load(chemistry = flux_gubbins, q = q, method = c('average'), aggregation = 'annual')})
    # expect_true(inherits(ms_load, 'try-error'))

    # # trying to use two aggregations
    # ms_load <- try({ms_calc_load(chemistry = chemistry, q = q, method = c('pw', 'rating'), aggregation = c('annual', 'monthly'))})
    # expect_true(inherits(ms_load, 'try-error'))
})

# test_that("dataframe returned with flux estimates which match same estimates made in Gubbins et al. 202X", {
#
#     chemistry <- ms_load_product(macrosheds_root = ms_root,
#                                  prodname = 'stream_chemistry',
#                                  site_codes = c('w6'),
#                                  filter_vars = c('Ca'))
#
#     q <- ms_load_product(macrosheds_root = ms_root,
#                          prodname = 'discharge',
#                          site_codes = c('w6'))
#
#     # input methods
#     test_methods <- c('rsfme')
#
#     # filter Gubbins flux to w6 Ca
#     ms_flux_gubbins <- flux_gubbins %>%
#         filter(var == 'Ca',
#                site_code == 'w6') %>%
#         arrange(wy, method)
#
#     # calc flux
#     ms_load <- ms_calc_load(chemistry = chemistry, q = q, method = test_methods, aggregation = 'annual') %>%
#         arrange(water_year, method)
#
#     # check random years for same values for methods
#     for(year in c(1964, 2010, 1978)){
#         expect_true(dplyr::all_equal(ms_load[ms_load$wy == year, ],
#                                      ms_flux_gubbins[ms_flux_gubbins$wy == year, ]))
#     }
#
#     # # check against published HBEF flux
#     # hbef_flux <- read_feather(file.path(flux_fp, 'hbef/stream_flux/w6.feather')) %>%
#     #     filter(var == 'GN_Ca') %>%
#     #     rename(water_year = wy, load = val) %>%
#     #     group_by(water_year) %>%
#     #     # sum and convert g to kg
#     #     summarize(load = sum(load) / 1000,
#     #               .groups = 'drop') %>%
#     #     mutate(water_year = as.numeric(water_year))
#     #
#     # ms_flux_summary <- ms_load %>%
#     #     group_by(water_year) %>%
#     #     summarize(load = mean(load))
#     #
#     # ms_difference <- ms_flux_summary %>%
#     #     left_join(hbef_flux, by = 'water_year') %>%
#     #     mutate(diff_pct = (load.x / load.y) * 100)
#     #
#     # # no flux estimates more than %50 different from HBEF published fluxes
#     # expect_true(all(ms_difference$diff_pct < 150 & ms_difference$diff_pct > 50))
# })

chemistry <- ms_load_product(macrosheds_root = ms_root,
                             prodname = 'stream_chemistry',
                             site_codes = c('w1', 'BARN'),
                             filter_vars = c('NO3_N', 'Cl')) %>%
    # choosing short, and weird time frame
    filter(date > "2011-08-01", date < "2013-04-01")

q <- ms_load_product(macrosheds_root = ms_root,
                     prodname = 'discharge',
                     site_codes = c('w1', 'BARN')) %>%
    filter(date > "2010-01-01")

ppt <- ms_load_product(macrosheds_root = ms_root,
                       prodname = 'precipitation',
                       site_codes = c('w1', 'BARN')) %>%
    filter(date > "2010-01-01")

test_that("testing all methods and aggregations permutations", {

    # input methods
    input_methods <- c('average', 'composite', 'pw', 'beale', 'rating')
    # input_methods_all <- c('average', 'composite', 'pw', 'beale', 'rating', 'simple') # should run only simple
    input_methods_advanced_abbv <- 'all'

    # aggregation options
    agg_options <- c('annual')
    # agg_options <- c('monthly', 'annual')

    # first, we check that all permutations of agg and method work (one method at a time)
    # for(agg in agg_options){
        for(input_method in input_methods){

            ms_load <- ms_calc_load(chemistry = chemistry, q = q,
                                    method = input_method)

            output_method <- sort(unique(ms_load$load$method))
            # check only user input methods are present in output "methods" column
            expect_identical(output_method, input_method)
        }
    # }
})
