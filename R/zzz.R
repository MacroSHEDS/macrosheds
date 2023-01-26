## .onAttach <- function(libname, pkgname) {
##   # load in macrosheds 'sysdata' from gihtub location
##   ms_sysdata_url <- "https://github.com/MacroSHEDS/macrosheds/raw/master/data/sysdata2.RData"
##   temp_dir <- tempdir()
##   temp_file_dom <- file.path(temp_dir, 'sysdata.Rdata')

##   download_status <- try(download.file(url = ms_sysdata_url,
##                                        destfile = temp_file_dom))

##   if(inherits(download_status, 'try-error')) {
##     warning('unable to download macrosheds sysdata for macrosheds data version',
##             '(cannot gaurantee user local macrosheds download functions will work)')
##   } else {
##     file_ids <- loadRData(temp_file_dom, verbose = TRUE)
##     ms_sysdata_github <- file_ids_for_r_package2
##     ms_sysdata_local <-

##   }




##   packageStartupMessage("This is version ", packageVersion(pkgname),
##                         " of ", pkgname, "\n",
##                         "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See ",
##                         "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing")
## }

## url("https://figshare.com/articles/dataset/watershed_summaries/16653064")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname), " of ", pkgname, "\n",
                        "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See ",
                        "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing")

  temp_dir <- tempdir()
  temp_file_dom <- paste0(temp_dir, 'test.feather')

  figshare_base <- 'https://figshare.com/ndownloader/files/'
  figshare_codes <- macrosheds::file_ids_for_r_package2 #loaded in R/sysdata2.rda

  test_info <- figshare_codes[which(figshare_codes$ut == 'watershed_summaries'),]
  test_url <- paste0(figshare_base, test_info[2])

  download_status <- try(download.file(url = test_url,
                                       destfile = temp_file_dom,
                                       quiet = TRUE,
                                       cacheOK = FALSE,
                                       mode = 'wb'))


  if(inherits(download_status, 'try-error')) {
    return('unable to download macrosheds data from figshare, local figshare IDs do not match latest dataset version.',
            'cannot gaurantee user local macrosheds download functions will work, please re-install macrosheds from github',
            'to ensure full functionality.')
  } else {
    return(invisible())
  }
}
