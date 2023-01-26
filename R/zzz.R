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
  
    # if connected, check whether figshare IDs are up to date
    if(is_online){
  
        id_check_url <- 'https://raw.githubusercontent.com/MacroSHEDS/macrosheds/master/data/figshare_id_check.txt'
        figshare_codes <- macrosheds::file_ids_for_r_package2 #loaded in R/sysdata2.rda
  
        test_id <- pull(figshare_codes[figshare_codes$ut == 'watershed_summaries', 'fig_code'])
        
        result <- try(readLines(id_check_url, 1), silent = TRUE)

        if(inherits(result, 'try-error') || ! inherits(result, 'character')){
            figshare_message <- paste(
                '-- macrosheds version check -- FAIL: could not check whether MacroSheds',
                'data download IDs are up to date. This should never happen, so please',
                'notify us at mail@macrosheds.org')
        } else if(result == test_id){
            figshare_message <- paste(
                '-- macrosheds version check -- SUCCESS: dataset connection tested',
                'and local macrosheds version has up-to-date data download IDs.')
        } else {
            figshare_message <- paste0(
                '-- macrosheds version check -- FAIL: local data download IDs do ',
                'not match latest dataset version. Please reinstall macrosheds with:\n',
                'devtools::install_github("https://github.com/MacroSHEDS/macrosheds.git")')
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
