library(testthat)
library(macrosheds)
options(timeout = 1200)

test_check('macrosheds', path='tests')
