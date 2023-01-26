.onAttach <- function(libname, pkgname) {
  # check if user has internet connection
  site <- "http://example.com/"
  is_online <- tryCatch(
    expr = {
      readLines(site, n=1)
      TRUE
    },
    # add handling for slow connection speed?
    warning = function(w) invokeRestart("muffleWarning"),
    error = function(e) FALSE
  )

  # if connected, check if user's local macrosheds figshare IDs
  # result in successful download
  if(is_online) {
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

      data_status <- try(feather::read_feather(temp_file_dom))


      # if download error
      if(inherits(data_status, 'try-error')) {
          figshare_message <- paste0('-- MacroSheds dataset version check -- FAIL unable to download macrosheds data from figshare, local figshare IDs do not match latest dataset version.',
                'cannot gaurantee user local macrosheds download functions will work, please re-install macrosheds from github',
                'to ensure full functionality.')
      } else {
      # if download success, assumed figshare IDs are accurate
          figshare_message <- '-- MacroSheds dataset version check -- SUCCESS dataset download connection tested and local macrosheds version has up-to-date IDs'
      }
  } else {
    figshare_message <- '-- MacroSheds dataset version check -- UNKNOWN no internet connection detetcted, dataset download connection untested'
  }

  # message
  packageStartupMessage(
    "\n\nThis is version ", packageVersion(pkgname), " of the ", pkgname, "R package \n\n",
    "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See \n",
    "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing",
    "\n\n",
    figshare_message
  )
}
