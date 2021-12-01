# example use:
#   chloride_data <- load_product(
#       macrosheds_root = '/path/to/macrosheds_dataset_v0.3',
#       prodname = 'stream_chemistry',
#       sort_result = TRUE,
#       filter_vars = 'Cl')

load_product <- function(macrosheds_root,
                         prodname,
                         sort_result = FALSE,
                         filter_vars){

    #These are the packages required by this function, but instead of
    #using library() or require() to load them, we'll use the DESCRIPTION file
    #require(tidyverse)
    #require(feather)
    #require(errors)

    #WARNING: this could easily eat up 20 GB RAM for a product like discharge.
    #As the dataset grows, that number will increase. This warning only applies
    #if the dataset version has full temporal granularity (not daily).

    # Units for each variable can be found in the variable catalog on macrosheds.org.

    # macrosheds_root: character. The path to the macrosheds dataset's parent
    #    directory, e.g. '~/stuff/macrosheds_dataset_v0.3'
    # prodname: character. read and combine all files associated with this prodname
    #    across all networks and domains. Available prodnames are:
    #    discharge, stream_chemistry, stream_flux_inst, precipitation,
    #    precip_chemistry, precip_flux_inst.
    # sort_result: logical. If TRUE, output will be sorted by site_code, var,
    #    datetime. this may take a few additional minutes for some products in
    #    the full 15m dataset.
    # filter_vars: character vector. for products like stream_chemistry that include
    #    multiple variables, this filters to just the ones specified (ignores
    #    variable prefixes). To see a catalog of variables, visit macrosheds.org

    list_all_product_dirs <- function(macrosheds_root, prodname){

        prodname_dirs <- list.dirs(path = macrosheds_root,
                                   full.names = TRUE,
                                   recursive = TRUE)

        prodname_dirs <- grep(pattern = paste0('derived/', prodname, '__'),
                              x = prodname_dirs,
                              value = TRUE)

        return(prodname_dirs)
    }

    drop_var_prefix <- function(x){

        unprefixed <- substr(x, 4, nchar(x))

        return(unprefixed)
    }

    avail_prodnames <- c('discharge', 'stream_chemistry', 'stream_flux_inst_scaled',
                         'precipitation', 'precip_chemistry', 'precip_flux_inst_scaled')

    if(! prodname %in% avail_prodnames){
        stop(paste0('prodname must be one of: ',
                    paste(avail_prodnames,
                          collapse = ', ')))
    }

    prodname_dirs <- list_all_product_dirs(macrosheds_root = macrosheds_root,
                                           prodname = prodname)

    d <- tibble()
    for(pd in prodname_dirs){

        rgx <- '/([a-zA-Z0-9\\-\\_]+)/([a-zA-Z0-9\\-\\_]+)/derived.+'
        network_domain <- str_match(string = pd,
                                    pattern = rgx)[, 2:3]

        d0 <- list.files(pd, full.names = TRUE) %>%
            purrr::map_dfr(read_feather)

        if(! missing(filter_vars)){
            d0 <- filter(d0,
                         drop_var_prefix(var) %in% filter_vars)
        }

        d <- d0 %>%
            mutate(val = errors::set_errors(val, val_err),
                   network = network_domain[1],
                   domain = network_domain[2]) %>%
            select(-val_err) %>%
            select(datetime, network, domain, site_code, var, val, ms_status,
                   ms_interp) %>%
            bind_rows(d)
    }

    if(nrow(d) == 0){

        if(missing(filter_vars)){
            stop('No results. Make sure macrosheds_root is correct.')
        } else {
            stop(paste('No results. Make sure macrosheds_root is correct and',
                       'filter_vars includes variable codes from the catalog',
                       'on macrosheds.org'))
        }
    }

    if(sort_result){
        d <- arrange(d,
                     site_code, var, datetime)
    }

    return(d)
}
