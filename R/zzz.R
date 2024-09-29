.onAttach <- function(libname, pkgname) {
    macrosheds_dataset_link <- "https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262"
      
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
  
        codecol <- colnames(figshare_codes)[-1] %>% 
            stringr::str_extract('[0-9\\.]+$') %>% 
            as.numeric() %>% 
            which.max() %>% 
            {. + 1}
        
        test_id <- pull(figshare_codes[figshare_codes$ut == 'watershed_summaries', codecol])
        
        result <- try(readLines(id_check_url, 1), silent = TRUE)

        if(inherits(result, 'try-error') || ! inherits(result, 'character')){
            figshare_message <- 'Failed to verify version is up to date. No connection?'
        } else if(result == test_id){
            figshare_message <- paste0('Dataset connection is up to date ', enc2native('\U2713'))
        } else {
            figshare_message <- paste0(enc2native('\U274C'), ' Package version is behind current dataset version. Update with ',
                'remotes::install_github("MacroSheds/macrosheds")')
        }
    } else {
        figshare_message <- 'Failed to verify version is up to date. No connection.'
    }
  
    packageStartupMessage(
        "\nmacrosheds v", packageVersion(pkgname), ": ", figshare_message, "\n\n",
        "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See \n",
        "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing",
        "\n\nComplete MacroSheds metadata: ", macrosheds_dataset_link
    )
}
