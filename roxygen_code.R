library(devtools)
library(roxygen2)

setwd('~/git/macrosheds/r_package/')
document(pkg = '.')
install('.')
# install.packages(path_to_file, repos = NULL, type="source")