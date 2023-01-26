.onAttach <- function(libname, pkgname) {
      
    # check if user has internet connection
    site <- "http://example.com/"
    is_online <- tryCatch(
        expr = {
            readLines(site, n = 1)
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
  
        test_info <- figshare_codes[figshare_codes$ut == 'watershed_summaries', 'fig_code']
        
        test_url <- paste0(figshare_base, test_info)
        
        result <- try(readChar(url(test_url), 1), silent = TRUE)

        # if download error
        if(inherits(result, 'try-error')) {
            figshare_message <- paste0('-- macrosheds version check -- FAIL: local data download IDs do not match latest dataset version.',
                  'Please run:\ndevtools::install_github("https://github.com/MacroSHEDS/macrosheds.git")')
        } else {
            # if download success, assume figshare IDs are accurate
            figshare_message <- '-- macrosheds version check -- SUCCESS: dataset connection tested and local macrosheds version has up-to-date data download IDs'
        }
    } else {
        figshare_message <- '-- macrosheds version check -- UNKNOWN: no internet connection detected.'
    }
  
    packageStartupMessage(
        "\n\nThis is version ", packageVersion(pkgname), " of ", pkgname, "\n\n",
        "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See \n",
        "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing",
        "\n\n",
        figshare_message
    )
}
