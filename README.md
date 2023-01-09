<!--
thank you "https://github.com/DouweHorsthuis/README-Template/blob/master/BLANK_README.md" for skeleton
-->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
<!-- [![LinkedIn][linkedin-shield]][linkedin-url] -->

<br>
<!-- <p align="center"> -->
  <!-- <a href="https://github.com/MacroSHEDS/macrosheds/"> -->
  <!--   <img src="images/logo.png" alt="macrosheds logo" width="160" height="80"> -->
  <!-- </a>  -->
<!-- <h3 align="center">MacroSheds</h3> -->
<!-- <h4 align="center">MacroSheds unites stream and watershed data from myriad public datasets onto one platform, making it easy to epxlore the hydrology, water quality, and biogeochemistry of rivers across North America and beyond</h4> -->

The **MacroSheds R package** provides functions for accessing, manipulating, and analyzing the MacroSheds dataset, a growing collection of stream and watershed data from across North America and beyond. 

While built for and around the MacroSheds dataset and project, many functions are also of general value, and can be applied to non-MacroSheds data. Functions are provided to help coax non-MacroSheds data into a suitable format for use with the MacroSheds dataset and package.

To learn more about the MacroSheds project, and to explore the data itself via interactive maps and visualizations, go to the [MacroSheds Web Portal](https://macrosheds.org).

**Table of Contents**
  
1. [About the project](#about-the-project)
2. [Getting started](#getting-started)
    - [Installation](#installation)  
3. [Usage](#usage)
4. [License](#license)
5. [Contact](#contact)
<!-- 3. [Acknowledgement](#acknowledgement) -->

<!-- ABOUT THE PROJECT -->
## About The MacroSheds Project


Watershed ecosystem science has identified plenty of idiosyncrasy within watersheds, but hasn't produced many general theories about watersheds at large. A major reason has been the challenge of data access and integration across all the organizations that house watershed data. (LTER, CZO/CZ Net, NEON, DOE, USFS, etc.).

MacroSheds unites stream and watershed data from all these sources on one platform, making it easy for anyone to explore the hydrology, water quality, and biogeochemistry of rivers across North America and beyond.

MacroSheds is funded by the U.S. National Science Foundation (Awd. 1926420). MacroSheds aquatic data are collected and provided by U.S. federally funded projects, with limited exception, see notes tab on the [MacroSheds Web Portal](https://macrosheds.org)

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

To use the MacroSheds R package a user must have at least R version 3.6.1 installed. We recommend the latest R version.

### Installation

```{r}
install.packages("devtools")
devtools::install_github("https://github.com/MacroSHEDS/macrosheds.git")
```

If you'll be using our watershed delineator `ms_delineate_watershed`, you'll also need to run

```{r}
whitebox::wbt_init()
whitebox::install_whitebox()
```

which initializes the WhiteboxTools geospatial backend and installs its Rust binaries that aren't included on CRAN. More info [here](https://giswqs.github.io/whiteboxR/).

<!-- USAGE EXAMPLES -->
## Usage

_For more information about how to use the MacroSheds R package, please refer to the [Vignettes](https://github.com/MacroSHEDS/macrosheds/tree/master/vignettes). To see the compiled vignettes through your browser, click on the *.html* or *.md* files, for example this [flux data retrieval and calculation  vignette](https://github.com/MacroSHEDS/macrosheds/blob/master/vignettes/ms_retrieval_flux_calc.md). The *.Rmd* version of these files can also be compiled using an R interpeter. 

_For more information about the MacroSheds Project, visit the [data portal](https://macrosheds.org)_

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

<!-- Your Name - [@twitter_handle](https://twitter.com/twitter_handle) - email -->

Project Link: [https://github.com/MacroSHEDS/macrosheds/](https://github.com/MacroSHEDS/macrosheds)

  - Spencer Rhea, spencerrhea41@gmail.com
  - Mike Vlah, michael.vlah@duke.edu
  - Wes Slaughter, weston.slaughter@duke.edu

<br><br>
<!-- ACKNOWLEDGEMENTS -->
<!-- ## Acknowledgements -->

<!-- * []() -->
<!-- * []() -->
<!-- * []() -->





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
