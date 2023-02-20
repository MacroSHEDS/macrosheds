This walkthrough is from the perspective of a first time user, with some
knowledge of the R programming language. This example user, for a
college project, needs to compare the flux of nitrate between two
watersheds, one from the Eastern US and one from the Western US, each
with at least 60 years of data.

Install and explore the `macrosheds` package
============================================

To use the `macrosheds` package, you must first **make sure you have the
`macrosheds` package on your computer, built, and installed in R**.
Currently, the package is only available via github. If you have not
already, you can install the `macrosheds` package as follows:

    # install.packages('devtools') #you may need to install devtools too

    devtools::install_github("https://github.com/MacroSHEDS/macrosheds")

Now, that we have the `macrosheds` package, we load it into our library,
along with other packages we will be using in this walkthrough.

    library(macrosheds)
    library(dplyr)
    library(ggplot2)

In this walkthrough, to learn more about the `macrosheds` package, its
functions, and how to use them, we will be using the `macrosheds`
package’s documentation.

Before anything else, we need to know what functions are available.
Let’s explore the package with `help()`

    help(package = macrosheds)

Identify target sites and variables in the MacroSheds dataset
=============================================================

Our first goal is to **identify sites we might want to pull data from**.
To get more information about MacroSheds sites, we will use the function
`download_ms_site_data`, which loads a tibble of MacroSheds site
metadata.

We will use `?` to get more information about the function, and how to
use it.

#### ms\_download\_site\_data

    ?ms_download_site_data

Looks like the function will run with no arugments, let’s run it and
look at the results.

    ms_sites <- ms_download_site_data()
    colnames(ms_sites)

    ##  [1] "network"          "network_fullname" "domain"           "domain_fullname" 
    ##  [5] "site_code"        "site_fullname"    "stream_name"      "site_type"       
    ##  [9] "ws_status"        "latitude"         "longitude"        "epsg_code"       
    ## [13] "ws_area_ha"       "n_observations"   "n_variables"      "first_record_utc"
    ## [17] "last_record_utc"  "timezone_olson"

Our objective is to find 2 watersheds, one in the Eastern US and one in
the Western US, which both have at least 60 years of nitrate data.

The `ms_sites` dataframe columns include “latitude”, “longitude”,
“first\_record\_utc”, and “last\_record\_utc”. Let’s use these to see
what sites meet our needs.

    # calculate the amount of days in sixty years
    sixty_yrs_days <- 365 * 60

    # filter the dataframe of macrosheds sites
    target_sites <- ms_sites %>%
        # to make sure our sites are roughly western or eastern 
        filter(longitude > -95 | longitude < -115) %>%
        mutate(period = as.numeric(
            difftime(last_record_utc,
                     first_record_utc,
                     units = "days"
                     ))) %>%
        # to get records > 60 years
        filter(period > sixty_yrs_days)

    # let's list the unique sites left after our filtering
    unique(target_sites$domain)

    ## [1] "hbef"      "hjandrews" "fernow"

    head(target_sites)

    ## # A tibble: 6 × 19
    ##   network network_fullname domain domain_fullname site_code site_fullname
    ##   <chr>   <chr>            <chr>  <chr>           <chr>     <chr>        
    ## 1 lter    LTER             hbef   Hubbard Brook   w1        Watershed 1  
    ## 2 lter    LTER             hbef   Hubbard Brook   w2        Watershed 2  
    ## 3 lter    LTER             hbef   Hubbard Brook   w3        Watershed 3  
    ## 4 lter    LTER             hbef   Hubbard Brook   w4        Watershed 4  
    ## 5 lter    LTER             hbef   Hubbard Brook   w5        Watershed 5  
    ## 6 lter    LTER             hbef   Hubbard Brook   w6        Watershed 6  
    ## # … with 13 more variables: stream_name <chr>, site_type <chr>,
    ## #   ws_status <chr>, latitude <dbl>, longitude <dbl>, epsg_code <dbl>,
    ## #   ws_area_ha <dbl>, n_observations <dbl>, n_variables <dbl>,
    ## #   first_record_utc <dttm>, last_record_utc <dttm>, timezone_olson <chr>,
    ## #   period <dbl>

    # and calculate summary statistics on these sites, to help
    # choose which sites have the best data for our needs
    target_summary <- target_sites %>%
        group_by(domain) %>%
        summarize(
            mean_long = mean(longitude),
            sum_obs = sum(n_observations),
            min_date = min(first_record_utc),
            max_date = min(last_record_utc)
        )
      
    head(target_summary)

    ## # A tibble: 3 × 5
    ##   domain    mean_long  sum_obs min_date            max_date           
    ##   <chr>         <dbl>    <dbl> <dttm>              <dttm>             
    ## 1 fernow        -79.7  1538665 1951-05-01 00:00:00 2020-12-31 00:00:00
    ## 2 hbef          -71.7 10260637 1956-01-01 00:00:00 2021-04-15 00:00:00
    ## 3 hjandrews    -122.   5659793 1949-10-01 00:00:00 2019-09-30 00:00:00

From our filter of the site metadata, we have found three domains which
have site data that meets our requirements. By summarizing the data,
we’ve found that two of these sites are in the Northeastern US (Fernow,
HBEF) and one is on the West Coast (HJ Andrews). Our objective is to
compare two sites, one east and one west, and between Fernow and HBEF
our summary data show that HBEF has a greater number of overall
observations, so let’s go with that.

    my_domains <- c("hbef", "hjandrews")

#### ms\_download\_variables

Now, we should choose the best site from HBEF and the best site from HJ
Andrews – in this case, which of each domain has the most complete
Nitrate record.

To get data for our target variable, nitrate, let’s make sure it’s in
the MacroSheds variable catalog, and see exactly what it is called. For
this, we will use `ms_download_variables`, which also does not need
arguments.

    ms_vars <- ms_catalog()

    ## Warning in ms_catalog(): NEON data present in catalog but not yet available in
    ## public version of dataset

    head(ms_vars)

    ## # A tibble: 6 × 10
    ##   variable_code variable_name        unit  network domain site_code observations
    ##   <chr>         <chr>                <chr> <chr>   <chr>  <chr>            <dbl>
    ## 1 abs250        UV Absorbance at 25… unit… neon    neon   ARIK              1171
    ## 2 abs250        UV Absorbance at 25… unit… neon    neon   BIGC               654
    ## 3 abs250        UV Absorbance at 25… unit… neon    neon   BLDE               617
    ## 4 abs250        UV Absorbance at 25… unit… neon    neon   BLUE              1051
    ## 5 abs250        UV Absorbance at 25… unit… neon    neon   BLWA              1170
    ## 6 abs250        UV Absorbance at 25… unit… neon    neon   CARI              1162
    ## # … with 3 more variables: first_record_utc <dttm>, last_record_utc <dttm>,
    ## #   mean_obs_per_day <dbl>

This is a pretty long list, let’s do a siple text search to see what
variable names start with “Nitrate”

    ms_NO3 <- ms_vars[grep("^Nitrate", ms_vars$variable_name),]

    # lets order the dataframe by number of obervations, and see whats at the top
    ms_NO3 <- ms_NO3[order(-ms_NO3$observations),]
    head(ms_NO3)

    ## # A tibble: 6 × 10
    ##   variable_code variable_name unit    network domain site_code observations
    ##   <chr>         <chr>         <chr>   <chr>   <chr>  <chr>            <dbl>
    ## 1 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N01B             54816
    ## 2 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N02B             54816
    ## 3 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N01B             46756
    ## 4 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N02B             46376
    ## 5 NO3_N         Nitrate-N     mg/L    <NA>    <NA>   w6               20526
    ## 6 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   w6               20526
    ## # … with 3 more variables: first_record_utc <dttm>, last_record_utc <dttm>,
    ## #   mean_obs_per_day <dbl>

    # Nitrate-N, with variable code NO3_N, tops the list
    # let's make sure that our domains have robust records
    m_NO3 <- ms_NO3 %>%
      filter(variable_code == "NO3_N") %>%
      filter(domain %in% my_domains)

    head(ms_NO3)

    ## # A tibble: 6 × 10
    ##   variable_code variable_name unit    network domain site_code observations
    ##   <chr>         <chr>         <chr>   <chr>   <chr>  <chr>            <dbl>
    ## 1 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N01B             54816
    ## 2 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N02B             54816
    ## 3 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N01B             46756
    ## 4 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   N02B             46376
    ## 5 NO3_N         Nitrate-N     mg/L    <NA>    <NA>   w6               20526
    ## 6 NO3_N         Nitrate-N     kg/ha/d <NA>    <NA>   w6               20526
    ## # … with 3 more variables: first_record_utc <dttm>, last_record_utc <dttm>,
    ## #   mean_obs_per_day <dbl>

Retrieve discharge data for target sites
========================================

Now, we will retrieve the target data: discharge and nitrate for our two
sites. First, we choose a directory to save the data to, and create a
vector of target domain names.

    my_ms_dir <- "./example/data"

We will also take a look at `ms_download_core_data` which we will sue to
retrieve all the available data for our domains of interest.

##### ms\_download\_core\_data

    ?ms_download_core_data

Now, we retrieve MacroSheds data by domain and save it to our directory.

    options(timeout = 600) #default 60 might not be enough if your connection is slow

    ms_download_core_data(
        my_ms_dir,
        domains = my_domains,
        quiet = FALSE
    )

##### ms\_load\_product *discharge*

Now we have the data downloaded, but we want to load specific variables
into the R session. To do this, we use `ms_load_product`.

    ?ms_load_product

Pointing to the directory we downloaded the data to, we use this
function to pull out discharge data for two sites.

    my_q <- ms_load_product(
        my_ms_dir,
        prodname = "discharge",
        site_codes = c("w1", "GSWS09"),
        sort_result = TRUE,
        warn = TRUE
    )

Retrieve stream chemistry data for target sites
===============================================

#### ms\_load\_product *chemistry*

Now that we have discharge, we will use `ms_load_product` again, but for
nitrate-nitrogen data. As we know from searching the variable catalog,
the code for nitrate-nitrogen is ‘NO3\_N’

    my_chem <- ms_load_product(
        my_ms_dir,
        prodname = "stream_chemistry",
        filter_vars = "NO3_N",
        site_codes = c("w1", "GSWS09"),
        sort_result = TRUE,
        warn = TRUE
    )

    head(my_chem)

    ## # A tibble: 6 × 7
    ##   datetime            site_code var        val ms_status ms_interp val_err
    ##   <dttm>              <chr>     <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 1968-10-09 00:00:00 GSWS09    GN_NO3_N     0         1         0       0
    ## 2 1968-10-10 00:00:00 GSWS09    GN_NO3_N     0         0         1       0
    ## 3 1968-10-11 00:00:00 GSWS09    GN_NO3_N     0         0         1       0
    ## 4 1968-10-12 00:00:00 GSWS09    GN_NO3_N     0         0         1       0
    ## 5 1968-10-13 00:00:00 GSWS09    GN_NO3_N     0         0         1       0
    ## 6 1968-10-14 00:00:00 GSWS09    GN_NO3_N     0         0         1       0

Manipulate and clean data
=========================

#### ms\_synchronize\_timestep

Now that we have our nitrate-nitrogen data, we have to correct for
differences in sampling frequency between the two sites. The MacroSheds
package helps user achieve this via the `ms_synchronize_timestep`
function.

    ?ms_synchronize_timestep

    my_chem_sync <- ms_synchronize_timestep(
        my_chem,
        desired_interval = "1 day",
        interpolate_limit = 40
    )

Calculate flux from this data
=============================

#### ms\_calc\_flux

Now that our nitrate-nitrogen data is pulled and synchronized, let’s use
`ms_calc_flux` to calculate nitrate-nitrogen flux from our discharge and
concentration datasets.

    ?ms_calc_flux

We can plug in our discharge and chemistry data directly.

    my_flux <- ms_calc_flux(
        chemistry = my_chem_sync,
        q = my_q,
        q_type = 'discharge',
        site_info = ms_sites
    )

    ## [1] "q dataset has a daily interval"

    head(my_flux)

    ## # A tibble: 6 × 7
    ##   datetime            site_code var        val ms_status ms_interp val_err
    ##   <dttm>              <chr>     <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 1968-10-09 00:00:00 GSWS09    GN_NO3_N     0         1         0       0
    ## 2 1968-10-10 00:00:00 GSWS09    GN_NO3_N     0         0         0       0
    ## 3 1968-10-11 00:00:00 GSWS09    GN_NO3_N     0         0         0       0
    ## 4 1968-10-12 00:00:00 GSWS09    GN_NO3_N     0         0         0       0
    ## 5 1968-10-13 00:00:00 GSWS09    GN_NO3_N     0         0         0       0
    ## 6 1968-10-14 00:00:00 GSWS09    GN_NO3_N     0         0         0       0

From the documentation of `ms_calc_flux` we know, that our flux units
are `kg/ha/T` where T is the sampling interval. This is ideal becuase we
are comparing between two very different watersheds and it is
appropriate to scale by area. But is you were intrested in clooking at
the mass of a solute being exported from a watershed, not normalized to
area, you can so with the MacroSheds function
‘ms\_undo\_scale\_flux\_by\_area’.

#### ms\_scale\_flux\_by\_area

    my_flux_unscaled <- ms_undo_scale_flux_by_area(my_flux)

Looks great! We have used the MacroSheds package and dataset to
retrieve, synchronize, scale, and calculate flux from streamflow and
nitrate-nitrogen concentration data from two sites on the east and west
coasts of the United States with &gt; 60 years of data. let’s plot it!

    my_annual_mean_flux <- my_flux %>%
        mutate(year = as.numeric(format(as.Date(datetime), format="%Y"))) %>%
        group_by(year, site_code) %>%
        summarize(
            mean_flux = sum(val),
            n = n(),
            .groups = 'drop',
        ) %>%
        filter(n > 330)

    ggplot(data=my_annual_mean_flux, aes(x=year, y=mean_flux, group=site_code)) +
        geom_line(aes(color=site_code), lwd = 2)+
        geom_point() +
        scale_color_manual(
          values=c("#E69F00", "#56B4E9"),
          name = "Site"
        ) +
        ggtitle(
          label = "Mean Annual Nitrate-Nitrogen Flux in an Eastern and Western Watershed",
          subtitle = "Hubbard Brook and HJ Andrews, 1970-2020"
        ) +
        xlab("Year") +
        ylab("Nitrate-Nitrogen Flux (kg/ha/day)") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 26),
          axis.title = element_text(size = 30),
          plot.title = element_text(size = 36),
          plot.subtitle = element_text(size = 32),
          legend.title = element_text(size = 36),
          legend.text = element_text(size = 30),
        ) 

![](https://raw.githubusercontent.com/westonslaughter/macrosheds/master/vignettes/files/nitrate_comparison.png)
