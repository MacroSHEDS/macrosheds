#' Read MacroSheds data products from disk into R
#'
#' Loads MacroSheds core time-series products and watershed attributes from the
#' MacroSheds dataset after components have been downloaded using [ms_download_core_data()]
#' or [ms_download_ws_attr()]. For watershed boundaries and gauge locations, see [ms_load_spatial_product()].
#' Complete documentation is available on [EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262).
#'
#' @author Spencer Rhea 
#' @author Mike Vlah, \email{vlahm13@@gmail.com}
#' @author Wes Slaughter
#' @param macrosheds_root character. The path to the MacroSheds dataset's parent
#'    directory, established with [ms_download_core_data()] and/or [ms_download_ws_attr()].
#'    If you specified different locations with each of these functions, you'll need to
#'    refer to them separately when loading time-series data vs. watershed attribute data.
#' @param prodname character. A MacroSheds product name. Files associated with this
#'    product name will be read and combined. See details for footnotes.
#'    Available prodnames are (for core time-series products):
#'
#' + discharge
#' + stream_chemistry
#' + stream_flux_inst_scaled*
#' + precipitation,
#' + precip_chemistry
#' + precip_flux_inst_scaled*
#' + stream_load_annual_scaled**
#'
#' (and for watershed attribute products):
#'
#' + ws_attr_summaries
#' + ws_attr_timeseries:all
#' + ws_attr_timeseries:climate
#' + ws_attr_timeseries:hydrology
#' + ws_attr_timeseries:landcover
#' + ws_attr_timeseries:parentmaterial
#' + ws_attr_timeseries:terrain
#' + ws_attr_timeseries:vegetation
#' + ws_attr_CAMELS_summaries
#' + ws_attr_CAMELS_Daymet_forcings
#' @param filter_vars character vector of variable codes. for products like stream_chemistry that include
#'    multiple variables, this filters to just the ones specified (ignores
#'    variable prefixes). Ignored if requesting discharge, precipitation, or watershed attributes.
#'    To see a catalog of variable codes, run [ms_load_variables()] or visit macrosheds.org.
#' @param networks,domains,site_codes character vectors of MacroSheds networks/domains/sites to load. Omit to load all.
#'    To see a catalog of site codes, run [ms_load_sites()] or visit [macrosheds.org].
#' @param sort_result logical. Ignored if requesting watershed attributes.
#'    If TRUE, and requesting core time-series data, output will be sorted by site_code, var,
#'    datetime. this may add considerable loading time for large datasets.
#' @param warn logical. If TRUE, function will not load more than 100 MB without permission.
#' @details
#'    *"instantaneous" (i.e. daily) flux, scaled by watershed area and reported in kg/ha/d.
#'    Note that flux products can also be computed from component products using
#'    [ms_calc_flux()] and [ms_calc_flux_rsfme()].
#'    **load = flux aggregated by water-year (Oct 1 to Sept 30). Load is computed by various methods,
#'    with MacroSheds-recommended method identified for each site_code-variable-water_year combination.
#'    Scaled by watershed area and reported in kg/ha/yr. See ms_calc_flux for method details.
#'    
#'    For some domains, it is possible to request "custom" variants of products, which
#'    are analogous to their standard types, but are calculated according to the methods
#'    employed by the primary source data provider. Examples are Panola stream flux, which
#'    can be loaded via `prodname = "CUSTOMstream_flux_inst_scaled"`. Consider these
#'    products to be "secret menu items" for now. You may identify them by searching for the
#'    word "CUSTOM" in your macrosheds_root directory. 
#' @return For all core timeseries products except load, returns a \code{tibble} in MacroSheds format.
#'    This format is often required in inputs to macrosheds tools, but usually only date, site_code, var, and val columns are required.
#' | header value  | column_definition |
#' | ------------- | ----------------- |
#' | date          | Date in YYYY-mm-dd |
#' | site_code     | A unique identifier for each MacroSheds site, identical to primary source site code where possible. See [ms_load_sites()]. |
#' | grab_sample   | Boolean integer indicating whether the observation was obtained via grab sample or installed sensor. 1 = TRUE (grab sample), 0 = FALSE (installed sensor). |
#' | var           | Variable code. See [ms_load_variables()]. |
#' | val           | Data value. See [ms_load_variables()] for units. |
#' | ms_status     | Boolean integer. 0 = clean value. 1 = questionable value. See "Technical Validation" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#' | ms_interp     | Boolean integer. 0 = measured or imputed by primary source. 1 = interpolated by MacroSheds. See "Temporal Imputation and Aggregation" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#' | val_err       | The combined standard uncertainty associated with the corresponding data point, if estimable. See "Detection Limits and Propagation of Uncertainty" section of [the MacroSheds data paper](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lol2.10325) for details. |
#'
#'   For other products, returns a tibble with one of several formats. Visit [EDI](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262).
#'   for complete column descriptions.
#' @export
#' @seealso [ms_download_core_data()], [ms_download_ws_attr()], [ms_load_spatial_product()], [ms_load_variables()], [ms_load_sites()]
#' @examples
#' ms_root = 'data/macrosheds'
#' ms_download_core_data(macrosheds_root = ms_root, domains = c('niwot', 'hjandrews'))
#' ms_download_ws_attr(macrosheds_root = ms_root, dataset = 'time series')
#' macrosheds_data <- ms_load_product(macrosheds_root = ms_root, 
#'                                    prodname = 'stream_chemistry', 
#'                                    filter_vars = 'NO3_N')
#' macrosheds_data <- ms_load_product(macrosheds_root = ms_root, 
#'                                    prodname = 'ws_attr_timeseries:all')

ms_load_product <- function(macrosheds_root, 
                            prodname,
                            filter_vars,
                            networks,
                            domains,
                            site_codes,
                            sort_result = FALSE,
                            warn = TRUE){

    library('dplyr', quietly = TRUE)

    requireNamespace('macrosheds', quietly = TRUE)
    
    # Checks
    avail_prodnames <- c('discharge', 'stream_chemistry',
                         'stream_flux_inst_scaled',
                         'precipitation', 'precip_chemistry',
                         'precip_flux_inst_scaled',
                         'stream_load_annual_scaled',
                         'ws_attr_summaries', 'ws_attr_timeseries:climate',
                         'ws_attr_timeseries:all',
                         'ws_attr_timeseries:hydrology', 'ws_attr_timeseries:landcover',
                         'ws_attr_timeseries:parentmaterial', 'ws_attr_timeseries:terrain',
                         'ws_attr_timeseries:vegetation', 'ws_attr_CAMELS_Daymet_forcings',
                         'ws_attr_CAMELS_summaries')

    if(missing(macrosheds_root)){
        stop('macrosheds_root must be supplied.')
    }
    if(! dir.exists(macrosheds_root)){
        stop('macrosheds_root does not exist. This should be the directory you specified when you ran ms_download_core_data or ms_download_ws_attr.')
    }
    if(missing(prodname)){
        stop('prodname must be supplied')
    }
    if(! inherits(prodname, 'character') || length(prodname) != 1){
        stop('prodname must be a character string')
    }
    if(! prodname %in% c(avail_prodnames, paste0('CUSTOM', avail_prodnames))){

        # if(grepl(prodname, '_flux_')){
        #     stop('Use ms_calc_flux_rsfme or ms_calc_flux to generate MacroSheds flux products.')
        # }

        stop(paste0('prodname must be one of: "',
                    paste(avail_prodnames, collapse = '", "'),
                    '".'))
    }
    
    if(prodname == 'stream_load_annual_scaled') prodname <- 'stream_load_scaled_annual'

    # if(grepl('flux', prodname)) {
    #   stop(paste('this flux product is not directly available through ms_load_product but can be',
    #            ' created using component MacroSheds products and ms_calc_flux. Use load product',
    #            ' to retrieve discharge and chemistry data, followed by ms_calc_flux.'))
    # }

    if(! missing(networks)){
        ntws <- unique(macrosheds::ms_site_data$network)
        if(any(! networks %in% ntws)){
            illeg <- Filter(function(x) ! x %in% ntws, networks)
            stop('Unknown network codes: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    if(! missing(domains)){
        dmns <- unique(macrosheds::ms_site_data$domain)
        if(any(! domains %in% dmns)){
            illeg <- Filter(function(x) ! x %in% dmns, domains)
            stop('Unknown domain codes: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    if(! missing(site_codes)){
        sits <- unique(macrosheds::ms_site_data$site_code)
        if(any(! site_codes %in% sits)){
            illeg <- Filter(function(x) ! x %in% sits, site_codes)
            stop('Unknown site codes: ', paste(illeg, collapse = ', '), '. see ms_load_sites().')
        }
    }
    
    # Fill in missing inputs with NULL
    if(missing(filter_vars)){
        filter_vars <- NULL
    }
    
    if(grepl('^ws_attr_', prodname)){
        
        if(prodname == 'ws_attr_summaries') msfile <- 'watershed_summaries.feather'
        if(prodname == 'ws_attr_CAMELS_Daymet_forcings') msfile <- 'Daymet_forcings_CAMELS.feather'
        if(prodname == 'ws_attr_CAMELS_summaries') msfile <- 'watershed_summaries_CAMELS.feather'
        
        if(grepl('^ws_attr_timeseries', prodname)){
            
            attr_set <- stringr::str_extract(prodname, '(?<=ws_attr_timeseries:).*')
            
            if(attr_set == 'all'){
                attr_set <- c('terrain', 'landcover', 'parentmaterial',
                              'vegetation', 'hydrology', 'climate')
                prodname <- paste0('ws_attr_timeseries:', attr_set)
            }

            msfile <- paste0('spatial_timeseries_', attr_set, '.feather')
        }
        msfile <- file.path(macrosheds_root, msfile)
        
        filecheck <- file.exists(msfile)
        if(! all(filecheck)){
            
            if(length(filecheck) > 1){
                prodname_err <- prodname[filecheck]
            } else {
                prodname_err <- prodname
            }
            
            stop('No file found for ',
                 paste(prodname_err, collapse = ', '),
                 '. Download with ms_download_ws_attr, or check your macrosheds_root.')
        }
        
        # Create size warning
        file_sizes <- sum(file.info(msfile)$size)
        file_sizes <- round(sum(file_sizes, na.rm = TRUE)/1000000, 1)
        
        if(warn && file_sizes > 1000){
            
            resp <- get_response_1char(msg = paste0('Unfiltered, this dataset would occupy about ',
                                                    file_sizes,
                                                    ' MB in memory. Do you want to continue? (y/n) > '),
                                       possible_chars = c('y', 'n'))
            
            if(resp == 'n'){
                message('Aborting dataset load. Sorry, but there\'s no easy way to make this one smaller. Contact us at mail@macrosheds.org')
                return(invisible())
            }
        }
        
        ntw_filt <- ! missing(networks)
        dmn_filt <- ! missing(domains)
        sit_filt <- ! missing(site_codes)
        
        o <- purrr::map_dfr(msfile, function(x){
                o_ <- feather::read_feather(x)
                
                if(ntw_filt) o_ <- filter(o_, network %in% networks)
                if(dmn_filt) o_ <- filter(o_, domain %in% domains)
                if(sit_filt) o_ <- filter(o_, site_code %in% site_codes)
                
                return(o_)
            })
        
        return(o)
    }
    
    prodname_dirs <- list_all_product_dirs(macrosheds_root = macrosheds_root,
                                           prodname = prodname)
    
    # List network files  
    if(! missing(networks)){
        netdom <- macrosheds::ms_site_data %>%
            dplyr::select(network, domain) %>%
            distinct(domain, .keep_all = TRUE)
        
        network_domains <- netdom %>%
            filter(network %in% !!networks) %>%
            pull(domain)
        
        # prodpaths_net <- grep(paste0(paste0(macrosheds_root, '/', network_domains, '/', prodname), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        
        prodpaths_net <- grep(paste(network_domains, collapse = '|'), prodname_dirs, value = TRUE)
        prodpaths_net <- list.files(prodpaths_net, full.names = TRUE)
    } else {
        prodpaths_net <- NULL
    }
    
    # List domains files 
    if(! missing(domains)){
        # prodpaths_dom <- grep(paste0(paste0(macrosheds_root, '/', domains, '/', prodname), 
        #                              collapse = '|'),
        #                       prodpaths, value = TRUE)
        prodpaths_dom <- grep(paste(domains, collapse = '|'), prodname_dirs, value = TRUE)
        prodpaths_dom <- list.files(prodpaths_dom, full.names = TRUE)
    } else {
        prodpaths_dom <- NULL
    }
    
    # List site files 
    all_files <- list.files(prodname_dirs, full.names = T)
    if(! missing(site_codes)){
        sites <- stringr::str_match(string = all_files,
                                    pattern = '([^/]+)(?=\\.feather$)')[,1]
        # prodpaths_sites <- all_files[grep(paste0(site_codes, collapse = '|'), sites)]
        prodpaths_sites <- all_files[sites %in% site_codes]
    } else {
        prodpaths_sites <- NULL
    }
    
    # Load all files 
    if(all(is.null(c(prodpaths_net, prodpaths_dom, prodpaths_sites)))){
        rel_files <- all_files
    } else {
        rel_files <- c(prodpaths_net, prodpaths_dom, prodpaths_sites)
    }
    rel_files <- unique(rel_files)
    rel_files <- grep('\\.feather$', rel_files, value = TRUE)

    # Create size warning
    file_sizes <- file.info(rel_files)$size
    file_sizes <- round(sum(file_sizes, na.rm = TRUE) / 1000000, 1)
    
    if(warn && file_sizes > 100 && is.null(filter_vars)){
        
        resp <- get_response_1char(msg = paste0('This dataset will occupy about ',
                                                file_sizes,
                                                ' MB in memory. Do you want to continue? (y/n) > '),
                                   possible_chars = c('y', 'n'))
        
        if(resp == 'n'){
            message('You can reduce dataset size by specifying network, domain, site_code, or filter_vars.')
            return(invisible())
        }
    }
    
    # Read in files 
    if(grepl('CUSTOM', rel_files[1])){
        
        d <- tibble()
        for(f in rel_files){
            
            flx_ <- stringr::str_extract(f, '([^/]+)/[^/]+\\.feather', group = 1)
            if(flx_ != prodname){
                dtl <- paste(prodname, flx_, sep = ':')
            } else {
                dtl <- prodname
            }
            
            d <- feather::read_feather(f) %>% 
                mutate(custom_product_designation = dtl) %>% 
                bind_rows(d)
        }
    } else {
        d <- purrr::map_dfr(rel_files, feather::read_feather)
    }
    
    if(nrow(d) == 0){
        stop(paste('No results. Check macrosheds_root and verify that desired networks/domains/sites have been',
                   'downloaded (see ms_download_core_data.) Some products do not exist for some domains.'))
    }
        
    if(! is.null(filter_vars)){
        
        fv_in_d <- filter_vars %in% d$var
        if(! any(fv_in_d)){
            stop(paste('None of filter_vars is present in this dataset'))
        }
        if(! all(fv_in_d)){
            warning(paste('These variables are not available in this dataset:',
                          filter_vars[! fv_in_d]))
        }
        filter_vars <- filter_vars[fv_in_d]
        
        d <- dplyr::filter(d, var %in% filter_vars)
    }
    
    if(sort_result){
        d <- arrange(d, site_code, var, datetime)
    }
    
    return(d)
}

