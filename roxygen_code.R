
# setwd('~/git/macrosheds/r_package/')

#address any errors here before building.
devtools::document('.') #preview won't render markdown-style Rd formatting
# devtools::check('.') #this runs all example code, so we can save it for CRAN submission
devtools::install('.')
