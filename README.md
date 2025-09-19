
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
<!--[![Issues][issues-shield]][issues-url]-->
[![MIT License][license-shield]][license-url]
<!-- [![LinkedIn][linkedin-shield]][linkedin-url] -->

<br>

# macrosheds

The **macrosheds R package** provides functions for accessing, manipulating, and analyzing the MacroSheds dataset, a growing collection of data from long term, small-watershed ecosystem studies across North America and beyond. 

 + [MacroSheds Data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325)
 + [Data dashboard for visualization](https://macrosheds.org)
 + [Dataset changelog](https://macrosheds.org/pages/changelog.html)
 + Questions: mail@macrosheds.org

<hr>
  
1. [Installation](#installation)  
2. [Usage](#usage)
3. [Changelog](#changelog)
4. [Issues](#issues)


# Installation

```{r}
install.packages("remotes")
remotes::install_github("https://github.com/MacroSHEDS/macrosheds.git")
```

If you'll be using our watershed delineator `ms_delineate_watershed`, you'll also need to run

```{r}
whitebox::wbt_init()
whitebox::install_whitebox()
```

which initializes the WhiteboxTools geospatial backend. More info [here](https://giswqs.github.io/whiteboxR/).

# Usage

```{r}
library(macrosheds)

ms_root <- 'path/to/wherever'

ms_sites <- ms_load_sites() #site metadata
ms_vars <- ms_load_variables('timeseries') #variable metadata

selected_domains <- c('niwot', 'hjandrews')
ms_download_core_data(ms_root, domains = selected_domains)

ms_download_ws_attr(ms_root, dataset = 'time series')

macrosheds_data <- ms_load_product(ms_root, prodname = 'stream_chemistry')
macrosheds_data <- ms_load_product(ms_root, prodname = 'discharge')
macrosheds_data <- ms_load_product(ms_root, prodname = 'stream_load_annual_scaled')
macrosheds_data <- ms_load_product(ms_root, prodname = 'ws_attr_timeseries:all')

#etc
```

For more examples, see the [Vignettes](https://macrosheds.org/pages/vignettes).

# Changelog

*v2* (2024-09-30):
 + Now able to retrieve any available version of the MacroSheds dataset. Dataset v2 is being published concurrently.
   + If you already had v1 files stored locally in `macrosheds_root`, these will be moved to `macrosheds_root/v1` the first time you use `ms_download_core_data`, `ms_download_ws_attr`, `ms_load_product`, or `ms_load_spatial_product`.
 + MacroSheds won't include sub-daily data any time soon, so the `datetime` column is now `date`.
 + We've done away with variable prefixes and added a Boolean `grab_sample` column to core time-series data. Watershed attribute sources and categories are in the variable metadata, which can be joined to any table.
 + We still report standard uncertainty via the `val_err` column, but none of the package functions attempt to carry it. It is there to be propagated on your own terms, if you so choose.
 + See the changelog for the MacroSheds dataset [here](https://macrosheds.org/pages/changelog.html).

# Issues

[https://github.com/MacroSHEDS/macrosheds/issues]

<br><br>

[contributors-shield]: https://cuahsi.shinyapps.io/macrosheds/_w_eb92b9c2/new_logo_full.png
[contributors-url]: https://github.com/MacroSHEDS/macrosheds/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/MacroSHEDS/macrosheds.svg?style=for-the-badge
[forks-url]: https://github.com/MacroSHEDS/macrosheds/network/members
[stars-shield]: https://img.shields.io/github/stars/MacroSHEDS/macrosheds.svg?style=for-the-badge
[stars-url]: https://github.com/MacroSHEDS/macrosheds/stargazers
[issues-shield]: https://img.shields.io/github/issues/MacroSHEDS/macrosheds.svg?style=for-the-badge
[issues-url]: https://github.com/MacroSHEDS/macrosheds/issues
[license-shield]: https://img.shields.io/github/license/MacroSHEDS/macrosheds.svg?style=for-the-badge
[license-url]: https://github.com/MacroSHEDS/macrosheds/blob/master/LICENSE.txt
<!-- [linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555 -->
<!-- [linkedin-url]: https://linkedin.com/in/linkedin_username -->
