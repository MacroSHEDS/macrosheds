## ----ms-install, eval = FALSE-------------------------------------------------
#  # install.packages('devtools') #you may need to install devtools too
#  
#  devtools::install_github('https://github.com/MacroSHEDS/macrosheds')

## ----setup-library, message = FALSE-------------------------------------------
   
library(macrosheds)
library(ggplot2)


## ----ms-help------------------------------------------------------------------
help(package = macrosheds)

## ----ms-site-data-------------------------------------------------------------
?ms_load_sites

## ----ms-sites-----------------------------------------------------------------
ms_sites <- ms_load_sites()
colnames(ms_sites)

## ----ms-site-filter-----------------------------------------------------------
# calculate the approximate number of days in sixty years
# sixty_yrs_days <- 365 * 60
# 
# # filter the dataframe of macrosheds sites
# target_sites <- ms_sites %>%
#     # to make sure our sites are roughly western or eastern 
#     filter(longitude > -95 | longitude < -115) %>%
#     mutate(period = as.numeric(
#         difftime(last_record_utc,
#                  first_record_utc,
#                  units = 'days'
#                  ))) %>%
#     # to get records > 60 years
#     filter(period > sixty_yrs_days)
# 
# # let's list the unique sites left after our filtering
# unique(target_sites$domain)
# head(target_sites)
#   
# # and calculate summary statistics on these sites, to help
# # choose which sites have the best data for our needs
# target_summary <- target_sites %>%
#     group_by(domain) %>%
#     summarize(
#         mean_long = mean(longitude),
#         sum_obs = sum(n_observations),
#         min_date = min(first_record_utc),
#         max_date = min(last_record_utc)
#     )
#   
# head(target_summary)

## -----------------------------------------------------------------------------
my_domains <- c('hbef', 'hjandrews')

## ----ms-vars------------------------------------------------------------------
ms_vars <- ms_load_variables(var_set = 'timeseries')
head(ms_vars)

## ----ms-vars-filter-----------------------------------------------------------
filter(ms_vars, grepl('nitrate', variable_name, ignore.case = TRUE))

# Nitrate-N of stream water has the most observations. The variable_code
# for that is 'NO3_N'. Let's see if our sites have that variable.

ms_sitevar_catalog <- ms_load_variables(var_set = 'timeseries_by_site')
ms_sitevar_catalog %>% 
    filter(domain %in% my_domains,
           variable_code == 'NO3_N',
           chem_category == 'stream_conc') %>% 
    arrange(desc(mean_obs_per_day), desc(observations))

#it looks like GSWS07 comes out on top for HJ Andrews, and w7 for HBEF.

## ----ms-dir-------------------------------------------------------------------
my_ms_dir <- './example/data'

## -----------------------------------------------------------------------------
?ms_download_core_data

