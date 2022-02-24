
# setwd('~/git/macrosheds/r_package/')

devtools::document(pkg = '.') #address any errors here before building
devtools::install('.')
