
library(macrosheds)
library(testthat)
library(readr)

#ms_root <- '../../data/ms_test' #superfluous files in the data/ directory causes problems for build.
ms_root <- '~/ssd2/ms_test' #so use a directory that works for your machine

options(timeout = 6000)

o = ms_generate_attribution(abide_by = 'suggestions')
tmpd = tempdir()
# macrosheds::ms_download_core_data(tmpd, domains = 'hbef')
macrosheds::ms_download_core_data(ms_root, domains = 'hbef')
d = macrosheds::ms_load_product(
    macrosheds_root = ms_root,
    # macrosheds_root = tmpd,
    prodname = 'discharge',
    domains = 'hbef')
o2 = ms_generate_attribution(d, chem_source = 'both',
                             write_to_dir = tmpd)
o3 = ms_generate_attribution(d, chem_source = 'both')

test_that('errors are intelligible', {
    expect_error(ms_generate_attribution(d = data.frame(y=1)), regexp = 'data\\.frame')
    expect_error(ms_generate_attribution(chem_source = 'a'), regexp = 'chem_source must')
    expect_error(ms_generate_attribution(d, include_all_ws_attr = 'b'), regexp = 'TRUE or')
    expect_error(ms_generate_attribution(abide_by = 'b'), regexp = 'abide_by must')
    expect_error(ms_generate_attribution(write_to_dir = 'test.txt'), regexp = 'does not exist')
})

test_that('output includes all expected components (no dataframe passed)', {
    expect_equal(names(o), c("acknowledgements", "bibliography", "intellectual_rights_notifications",
                             "intellectual_rights_explanations", "full_details_timeseries", "full_details_ws_attr"))
})

test_that('full acknowledgements properly formatted', {
    expect_equal(length(o$acknowledgements), 1)
    expect_gt(nchar(o$acknowledgements), 100)
    expect_match(o$acknowledgements, '^Primary data')
    expect_match(o$acknowledgements, 'Bear Brook \\(Maine\\)')
})

test_that('full bibliography properly formatted', {
    expect_gt(length(o$bibliography), 340)
    expect_true(all(substr(o$bibliography, 1, 1) == '@'))
    expect_true(all(grepl('title', o$bibliography)))
})

test_that('IR notifications properly formatted', {
    expect_equal(names(o$intellectual_rights_notifications),
                 c("noncommercial_license", "sharealike_license", "notify_of_intent_S",
                   "notify_of_intent_M", "notify_on_distribution_S",
                   "notify_on_distribution_M", "provide_access_S", "provide_access_M",
                   "consult_or_collab_S"))
    expect_true(all(sapply(o$intellectual_rights_notifications, nrow)))
})

test_that('IR explanations properly formatted', {
    expect_equal(length(o$intellectual_rights_explanations), 9)
    expect_true(all(sapply(o$intellectual_rights_explanations, is.character)))
})

test_that('IR details properly formatted', {
    expect_gt(nrow(o$full_details_timeseries), 380)
    expect_setequal(unique(o$full_details_timeseries$domain),
                    c('bear', 'boulder', 'calhoun', 'catalina_jemez', 'shale_hills',
                      'east_river', 'walker_branch', 'krycklan', 'arctic', 'baltimore',
                      'bonanza', 'hbef', 'hjandrews', 'konza', 'luquillo', 'mcmurdo', 'niwot',
                      'plum', 'santa_barbara', 'fernow', 'krew', 'santee', 'suef', 'sleepers',
                      'neon', 'panola', 'trout_lake', 'loch_vale', 'usgs'))
    expect_equal(ncol(o$full_details_ws_attr), 8)
    expect_gt(nrow(o$full_details_ws_attr), 5)
})

test_that('filtering done properly', {
    expect_equal(length(o3$acknowledgements), 1)
    expect_lt(length(o3$bibliography), 30)
    expect_length(o3$intellectual_rights_notifications, 0)
    expect_length(o3$intellectual_rights_explanations, 0)
    expect_equal(nrow(o3$full_details_timeseries), 4)
})

test_that('files written properly', {
    expect_equal(length(o2), 2)
    expect_length(read_lines(file.path(tmpd, 'macrosheds_attribution_information', 'acknowledgements.txt')), 2)
    expect_false(file.exists(file.path(tmpd, 'macrosheds_attribution_information', 'intellectual_rights_definitions.txt')))
    expect_false(file.exists(file.path(tmpd, 'macrosheds_attribution_information', 'intellectual_rights_notifications.txt')))
    expect_gt(length(read_lines(file.path(tmpd, 'macrosheds_attribution_information', 'ms_bibliography.bib'))), 40)
    expect_lt(length(read_lines(file.path(tmpd, 'macrosheds_attribution_information', 'ms_bibliography.bib'))), 50)
})

test_that('all ws_attr can be passed in piecemeal', {

    d <- dplyr::select(ms_load_variables('ws_attr'), var = variable_code) %>%
        mutate(site_code = 'w1')
    zz <- ms_generate_attribution(d)
    expect_equal(nrow(zz$full_details_ws_attr), 21)
})
