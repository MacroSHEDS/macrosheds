#' Internal functions
#' 
#' Not intended to be called directly by the user
#' 
#' @keywords internal 

# Function aliases 
sw = suppressWarnings
sm = suppressWarnings
`%dopar%` <- foreach::`%dopar%`
`%do%` <- foreach::`%do%`
# end function aliases 

list_all_product_dirs <- function(macrosheds_root, prodname){
    
    prodname_dirs <- list.dirs(path = macrosheds_root,
                               full.names = TRUE,
                               recursive = TRUE)
    
    prodname_dirs <- grep(pattern = paste0(prodname),
                          x = prodname_dirs,
                          value = TRUE)
    
    return(prodname_dirs)
}

Mode <- function(x, na.rm = TRUE){
    
    if(na.rm){
        x <- na.omit(x)
    }
    
    ux <- unique(x)
    mode_out <- ux[which.max(tabulate(match(x, ux)))]
    return(mode_out)
    
}

# Unit conversion functions 
parse_molecular_formulae <- function(formulae){
    
    #`formulae` is a vector
    
    # formulae = c('C', 'C4', 'Cl', 'Cl2', 'CCl', 'C2Cl', 'C2Cl2', 'C2Cl2B2')
    # formulae = 'BCH10He10PLi2'
    # formulae='Mn'
    
    conc_vars = str_match(formulae, '^(?:OM|TM|DO|TD|UT|UTK|TK|TI|TO|DI)?([A-Za-z0-9]+)_?')[,2]
    two_let_symb_num = str_extract_all(conc_vars, '([A-Z][a-z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z][0-9]+)')
    one_let_symb_num = str_extract_all(conc_vars, '([A-Z][0-9]+)')
    conc_vars = str_remove_all(conc_vars, '([A-Z][0-9]+)')
    two_let_symb = str_extract_all(conc_vars, '([A-Z][a-z])')
    conc_vars = str_remove_all(conc_vars, '([A-Z][a-z])')
    one_let_symb = str_extract_all(conc_vars, '([A-Z])')
    
    constituents = mapply(c, SIMPLIFY=FALSE,
                          two_let_symb_num, one_let_symb_num, two_let_symb, one_let_symb)
    
    return(constituents) # a list of vectors
}

combine_atomic_masses <- function(molecular_constituents){
    
    #`molecular_constituents` is a vector
    
    xmat = str_match(molecular_constituents,
                     '([A-Z][a-z]?)([0-9]+)?')[, -1, drop=FALSE]
    elems = xmat[,1]
    mults = as.numeric(xmat[,2])
    mults[is.na(mults)] = 1
    molecular_mass = sum(PeriodicTable::mass(elems) * mults)
    
    return(molecular_mass) #a scalar
}

calculate_molar_mass <- function(molecular_formula){
    
    if(length(molecular_formula) > 1){
        stop('molecular_formula must be a string of length 1')
    }
    
    parsed_formula = parse_molecular_formulae(molecular_formula)[[1]]
    
    molar_mass = combine_atomic_masses(parsed_formula)
    
    return(molar_mass)
}

convert_molecule <- function(x, from, to){
    
    #e.g. convert_molecule(1.54, 'NH4', 'N')
    
    molecule_real <- ms_vars %>%
        filter(variable_code == !!from) %>%
        pull(molecule)
    
    if(!is.na(molecule_real)) {
        from <- molecule_real
    }
    
    from_mass <- calculate_molar_mass(from)
    to_mass <- calculate_molar_mass(to)
    converted_mass <- x * to_mass / from_mass
    
    return(converted_mass)
}

convert_to_gl <- function(x, input_unit, molecule) {
    
    molecule_real <- ms_vars %>%
        filter(variable_code == !!molecule) %>%
        pull(molecule)
    
    if(!is.na(molecule_real)) {
        formula <- molecule_real
    } else {
        formula <- molecule
    }
    
    if(grepl('eq', input_unit)) {
        valence = ms_vars$valence[ms_vars$variable_code %in% molecule]
        
        if(length(valence) == 0) {stop('Varible is likely missing from ms_vars')}
        x = (x * calculate_molar_mass(formula)) / valence
        
        return(x)
    }
    
    if(grepl('mol', input_unit)) {
        x = x * calculate_molar_mass(formula)
        
        return(x)
    }
    
    return(x)
    
}

convert_from_gl <- function(x, input_unit, output_unit, molecule, g_conver) {
    
    molecule_real <- ms_vars %>%
        filter(variable_code == !!molecule) %>%
        pull(molecule)
    
    if(!is.na(molecule_real)) {
        formula <- molecule_real
    } else {
        formula <- molecule
    }
    
    if(grepl('eq', output_unit) && grepl('g', input_unit) ||
       grepl('eq', output_unit) && g_conver) {
        
        valence = ms_vars$valence[ms_vars$variable_code %in% molecule]
        if(length(valence) == 0 | is.na(valence)) {stop('Varible is likely missing from ms_vars')}
        x = (x * valence) / calculate_molar_mass(formula)
        
        return(x)
    }
    
    if(grepl('mol', output_unit) && grepl('g', input_unit) ||
       grepl('mol', output_unit) && g_conver) {
        
        x = x / calculate_molar_mass(formula)
        
        return(x)
    }
    
    if(grepl('mol', output_unit) && grepl('eq', input_unit) && !g_conver) {
        
        valence = ms_vars$valence[ms_vars$variable_code %in% molecule]
        if(length(valence) == 0) {stop('Varible is likely missing from ms_vars')}
        x = (x * calculate_molar_mass(formula)) / valence
        
        x = x / calculate_molar_mass(formula)
        
        return(x)
    }
    
    if(grepl('eq', output_unit) && grepl('mol', input_unit) && !g_conver) {
        
        x = x * calculate_molar_mass(formula)
        
        valence = ms_vars$valence[ms_vars$variable_code %in% molecule]
        if(length(valence) == 0) {stop('Varible is likely missing from ms_vars')}
        x = (x * valence)/calculate_molar_mass(formula)
        
        return(x)
    }
    
    return(x)
    
}

convert_unit <- function(x, input_unit, output_unit){
    
    units <- tibble(prefix = c('n', "u", "m", "c", "d", "h", "k", "M"),
                    convert_factor = c(0.000000001, 0.000001, 0.001, 0.01, 0.1, 100,
                                       1000, 1000000))
    
    old_fraction <- as.vector(str_split_fixed(input_unit, "/", n = Inf))
    old_top <- as.vector(str_split_fixed(old_fraction[1], "", n = Inf))
    
    if(length(old_fraction) == 2) {
        old_bottom <- as.vector(str_split_fixed(old_fraction[2], "", n = Inf))
    }
    
    new_fraction <- as.vector(str_split_fixed(output_unit, "/", n = Inf))
    new_top <- as.vector(str_split_fixed(new_fraction[1], "", n = Inf))
    
    if(length(new_fraction == 2)) {
        new_bottom <- as.vector(str_split_fixed(new_fraction[2], "", n = Inf))
    }
    
    old_top_unit <- tolower(str_split_fixed(old_top, "", 2)[1])
    
    if(old_top_unit %in% c('g', 'e', 'q', 'l') || old_fraction[1] == 'mol') {
        old_top_conver <- 1
    } else {
        old_top_conver <- as.numeric(filter(units, prefix == old_top_unit)[,2])
    }
    
    old_bottom_unit <- tolower(str_split_fixed(old_bottom, "", 2)[1])
    
    if(old_bottom_unit %in% c('g', 'e', 'q', 'l') || old_fraction[2] == 'mol') {
        old_bottom_conver <- 1
    } else {
        old_bottom_conver <- as.numeric(filter(units, prefix == old_bottom_unit)[,2])
    }
    
    new_top_unit <- tolower(str_split_fixed(new_top, "", 2)[1])
    
    if(new_top_unit %in% c('g', 'e', 'q', 'l') || new_fraction[1] == 'mol') {
        new_top_conver <- 1
    } else {
        new_top_conver <- as.numeric(filter(units, prefix == new_top_unit)[,2])
    }
    
    new_bottom_unit <- tolower(str_split_fixed(new_bottom, "", 2)[1])
    
    if(new_bottom_unit %in% c('g', 'e', 'q', 'l') || new_fraction[2] == 'mol') {
        new_bottom_conver <- 1
    } else {
        new_bottom_conver <- as.numeric(filter(units, prefix == new_bottom_unit)[,2])
    }
    
    new_val <- x*old_top_conver
    new_val <- new_val/new_top_conver
    
    new_val <- new_val/old_bottom_conver
    new_val <- new_val*new_bottom_conver
    
    return(new_val)
}
# End unit converstion
sd_or_0 <- function(x, na.rm = FALSE){
    
    #Only used to bypass the tyranny of the errors package not letting
    #me take the mean of an errors object of length 1 without setting the
    #uncertainty to 0
    
    x <- if(is.vector(x) || is.factor(x)) x else as.double(x)
    
    if(length(x) == 1) return(0)
    
    x <- sqrt(var(x, na.rm = na.rm))
}


populate_implicit_NAs <- function(d,
                                  interval,
                                  val_fill = NA,
                                  edges_only = FALSE){
    
    #TODO: this would be more flexible if we could pass column names as
    #   positional args and use them in group_by and mutate
    
    #d: a ms tibble with at minimum datetime, site_code, and var columns
    #interval: the interval along which to populate missing values. (must be
    #   either '15 min' or '1 day'.
    #val_fill: character or NA. the token with which to populate missing
    #   elements of the `val` column. All other columns will be populated
    #   invariably with NA or 0. See details.
    #edges_only: logical. if TRUE, only two filler rows will be inserted into each
    #   gap, one just after the gap begins and the other just before the gap ends.
    #   If FALSE (the default), the gap will be fully populated according to
    #   the methods outlined in the details section.
    
    #this function makes implicit missing timeseries records explicit,
    #   by populating rows so that the datetime column is complete
    #   with respect to the sampling interval. In other words, if
    #   samples are taken every 15 minutes, but some samples are skipped
    #   (rows not present), this will create those rows. If ms_status or
    #   ms_interp columns are present, their new records will be populated
    #   with 0s. The val column will be populated with whatever is passed to
    #   val_fill. Any other columns will be populated with NAs.
    
    #returns d, complete with new rows, sorted by site_code, then var, then datetime

    
    complete_d <- d %>%
        mutate(fill_marker = 1) %>%
        group_by(site_code, var) %>%
        tidyr::complete(datetime = seq(min(datetime),
                                       max(datetime),
                                       by = interval)) %>%
        # mutate(site_code = .$site_code[1],
        #        var = .$var[1]) %>%
        ungroup() %>%
        arrange(site_code, var, datetime) %>%
        select(datetime, site_code, var, everything())
    
    if(! any(is.na(complete_d$fill_marker))) return(d)
    
    if(! is.na(val_fill)){
        complete_d$val[is.na(complete_d$fill_marker)] <- val_fill
    }
    
    if('ms_status' %in% colnames(complete_d)){
        complete_d$ms_status[is.na(complete_d$ms_status)] <- 0
    }
    
    if('ms_interp' %in% colnames(complete_d)){
        complete_d$ms_interp[is.na(complete_d$ms_interp)] <- 0
    }
    
    if(edges_only){
        
        midgap_rows <- rle2(is.na(complete_d$fill_marker)) %>%
            filter(values == TRUE) %>%
            # if(nrow(fill_runs) == 0) return(d)
            # midgap_rows <- fill_runs %>%
            select(starts, stops) %>%
            {purrr::map2(.x = .$starts,
                         .y = .$stops,
                         ~seq(.x, .y))} %>%
            purrr::map(~( if(length(.x) <= 2)
            {
                return(NULL)
            } else {
                return(.x[2:(length(.x) - 1)])
            }
            )) %>%
            unlist()
        if(! is.null(midgap_rows)){
            complete_d <- slice(complete_d,
                                -midgap_rows)
        }
    }
    
    complete_d$fill_marker <- NULL
    
    return(complete_d)
}

ms_linear_interpolate <- function(d, interval, max_samples_to_impute){
    
    #d: a ms tibble with no ms_interp column (this will be created)
    #interval: the sampling interval (either '15 min' or '1 day'). an
    #   appropriate maxgap (i.e. max number of consecutive NAs to fill) will
    #   be chosen based on this interval.
    
    #fills gaps up to maxgap (determined automatically), then removes missing values
    
    #TODO: prefer imputeTS::na_seadec when there are >=2 non-NA datapoints.
    #   There are commented sections that begin this work, but we still would
    #   need to calculate start and end when creating a ts() object. we'd
    #   also need to separate uncertainty from the val column before converting
    #   to ts. here is the line that could be added to this documentation
    #   if we ever implement na_seadec:
    #For linear interpolation with
    #   seasonal decomposition, interval will also be used to determine
    #   the fraction of the sampling period between samples.
    
    if(length(unique(d$site_code)) > 1){
        stop(paste('ms_linear_interpolate is not designed to handle datasets',
                   'with more than one site.'))
    }
    
    if(length(unique(d$var)) > 1){
        stop(paste('ms_linear_interpolate is not designed to handle datasets',
                   'with more than one variable'))
    }
    
    var <- macrosheds::ms_drop_var_prefix(d$var[1])
    
    d <- arrange(d, datetime)
    ms_interp_column <- is.na(d$val)
    
    d_interp <- d %>%
        mutate(
            #carry ms_status to any rows that have just been populated (probably
            #redundant now, but can't hurt)
            ms_status <- imputeTS::na_locf(ms_status,
                                           na_remaining = 'rev'),
            val = if(sum(! is.na(val)) > 1){
                #linear interp NA vals
                imputeTS::na_interpolation(val,
                                           maxgap = max_samples_to_impute)
                #unless not enough data in group; then do nothing
            } else val,
            val_err = if(sum(! is.na(val_err)) > 1){
                #linear interp NA vals
                imputeTS::na_interpolation(val_err,
                                           maxgap = max_samples_to_impute)
                #unless not enough data in group; then do nothing
            } else val_err
        ) %>%
        select(any_of(c('datetime', 'site_code', 'var', 'val', 'ms_status', 'ms_interp', 'val_err'))) %>%
        arrange(site_code, var, datetime)
    
    ms_interp_column <- ms_interp_column & ! is.na(d_interp$val)
    d_interp$ms_interp <- as.numeric(ms_interp_column)
    d_interp <- filter(d_interp,
                       ! is.na(val))
    
    return(d_interp)
}

numeric_any <- function(num_vec){
    return(as.numeric(any(as.logical(num_vec))))
}

# EGRET stuff 
get_days_since_1850 <- function(dates){
    return_dates <- as.numeric(as_date(dates)-lubridate::ymd('1850-01-01'))
    return(return_dates)
}
get_DecYear <- function(dates){
    
    year <- lubridate::year(dates)
    
    get_days <- function(year){
        if((year %% 4) == 0) {
            if((year %% 100) == 0) {
                if((year %% 400) == 0) {
                    day_years <- 366
                } else {
                    day_years <- 365
                }
            } else {
                day_years <- 366
            }
        } else {
            day_years <- 365
        }
    }
    
    days_in_year <- map_dbl(year, get_days)
    
    DecYear <- (lubridate::yday(dates)/days_in_year)+lubridate::year(dates)
    
    return(DecYear)
}
get_MonthSeq <- function(dates){
    
    years <- lubridate::year(dates)-1850
    
    MonthSeq <- years*12
    
    MonthSeq <- MonthSeq + lubridate::month(dates)
    
    return(MonthSeq)
}
get_start_end <- function(d){
    start_date <- min(d$datetime)
    start_year <- lubridate::year(start_date)
    start_wy <- ifelse(lubridate::month(start_date) %in% c(10, 11, 12), start_year+1, start_year)
    filter_start_date <- lubridate::ymd(paste0(start_wy, '-10-01'))
    
    end_date <- max(d$datetime)
    end_year <- lubridate::year(end_date)
    end_wy <- ifelse(lubridate::month(end_date) %in% c(10, 11, 12), end_year+1, end_year)
    filter_end_date <- lubridate::ymd(paste0(end_wy, '-10-01'))
    
    fin_dates <- c(filter_start_date, filter_end_date)
    return(fin_dates)
    
}
drop_var_prefix <- function(x){
    
    unprefixed <- substr(x, 4, nchar(x))
    
    return(unprefixed)
}
# From EGRET
decimalDate <- function(rawData){
    
    dateTime <- as.POSIXlt(rawData)
    year <- dateTime$year + 1900
    
    startYear <- as.POSIXct(paste0(year,"-01-01 00:00"))
    endYear <- as.POSIXct(paste0(year+1,"-01-01 00:00"))
    
    DecYear <- year + as.numeric(difftime(dateTime, startYear, units = "secs"))/as.numeric(difftime(endYear, startYear, units = "secs"))
    return(DecYear)
}
# End EGRET stuff


# Precip interpolation stuff 
read_combine_shapefiles <- function(network, domain, prodname_ms){
    
    #TODO: sometimes multiple locations are listed for the same rain gauge.
    #   if there's a date associated with those locations, we should take that
    #   into account when performing IDW, and when plotting gauges on the map.
    #   (maybe previous locations could show up semitransparent). for now,
    #   we're just grabbing the last location listed (by order). We don't store
    #   date columns with our raingauge shapes yet, so that's the place to start.
    
    #TODO: also, this may be a problem for other spatial stuff.
    
    level <- ifelse(is_derived_product(prodname_ms),
                    'derived',
                    'munged')
    
    prodpaths <- list.files(glue::glue('data/{n}/{d}/{l}/{p}',
                                 n = network,
                                 d = domain,
                                 l = level,
                                 p = prodname_ms),
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = '*.shp')
    
    shapes <- lapply(prodpaths,
                     function(x){
                         sf::st_read(x,
                                     stringsAsFactors = FALSE,
                                     quiet = TRUE) %>%
                             slice_tail()
                     })
    
    # wb <- sw(Reduce(sf::st_union, wbs)) %>%
    combined <- sw(Reduce(bind_rows, shapes))
    # sf::st_transform(projstring)
    
    return(combined)
}

read_combine_feathers <- function(network,
                                  domain,
                                  prodname_ms){
    
    #read all data feathers associated with a network-domain-product,
    #row bind them, arrange by site_code, var, datetime. insert val_err column
    #into the val column as errors attribute and then remove val_err column
    #(error/uncertainty is handled by the errors package as an attribute,
    #so it must be written/read as a separate column).
    
    #the processing level is determined automatically from prodname_ms.
    #   If the product code is "msXXX" where X is a numeral, the processing
    #   level is assumed to be "derived". otherwise "munged"
    
    #handled in ms_list_files
    # level <- ifelse(is_derived_product(prodname_ms),
    #                 'derived',
    #                 'munged')
    
    prodpaths <- ms_list_files(network = network,
                               domain = domain,
                               prodname_ms = prodname_ms)
    
    combined <- tibble()
    for(i in 1:length(prodpaths)){
        part <- read_feather(prodpaths[i])
        combined <- bind_rows(combined, part)
    }
    
    combined <- combined %>%
        mutate(val = errors::set_errors(val, val_err)) %>%
        select(-val_err) %>%
        arrange(site_code, var, datetime)
    
    return(combined)
}

datetimes_to_durations <- function(datetime_vec,
                                   variable_prefix_vec = NULL,
                                   unit,
                                   sensor_maxgap = Inf,
                                   nonsensor_maxgap = Inf,
                                   grab_maxgap = Inf,
                                   installed_maxgap = Inf){
    
    #datetime_vec: POSIXct. a vector of datetimes
    #variable_prefix_vec: POSIXct. a vector of macrosheds variable prefixes,
    #   e.g. 'IS'. This vector is generated by calling extract_var_prefix
    #   on the var column of a macrosheds data.frame. Only required if you
    #   supply one or more of the maxgap parameters.
    #unit: string. The desired datetime unit. Must be expressed in a form
    #   that's readable by base::difftime
    #sensor_maxgap: numeric. the largest data gap that should return
    #   a value. Durations longer than this gap will return NA. Expressed in
    #   the same units as unit (see above). This applies
    #   to sensor data only (IS and GS).
    #nonsensor_maxgap: numeric. the largest data gap that should return
    #   a value. Durations longer than this gap will return NA. Expressed in
    #   the same units as unit (see above). This applies
    #   to nonsensor data only (IN and GN).
    #grab_maxgap: numeric. the largest data gap that should return
    #   a value. Durations longer than this gap will return NA. Expressed in
    #   the same units as unit (see above). This applies
    #   to grab data only (GS and GN).
    #installed_maxgap: numeric. the largest data gap that should return
    #   a value. Durations longer than this gap will return NA. Expressed in
    #   the same units as unit (see above). This applies
    #   only to data from installed units (IS and IN).
    
    #an NA is prepended to the output, so that its length is the same as
    #   datetime_vec
    
    if(! is.null(variable_prefix_vec) &&
       ! all(variable_prefix_vec %in% c('GN', 'GS', 'IN', 'IS'))){
        stop(paste('all elements of variable_prefix_vec must be one of "GN",',
                   '"GS", "IN", "IS"'))
    }
    
    durs <- diff(datetime_vec)
    units(durs) <- unit
    durs <- c(NA_real_, as.numeric(durs))
    
    if(! is.null(variable_prefix_vec)){
        is_sensor_data <- grepl('^.S', variable_prefix_vec)
        is_installed_data <- grepl('^I', variable_prefix_vec)
    }
    
    if(! is.infinite(sensor_maxgap)){
        durs[is_sensor_data & durs > sensor_maxgap] <- NA_real_
    }
    if(! is.infinite(nonsensor_maxgap)){
        durs[! is_sensor_data & durs > nonsensor_maxgap] <- NA_real_
    }
    if(! is.infinite(grab_maxgap)){
        durs[! is_installed_data & durs > grab_maxgap] <- NA_real_
    }
    if(! is.infinite(installed_maxgap)){
        durs[is_installed_data & durs > installed_maxgap] <- NA_real_
    }
    
    return(durs)
}

chunk_df <- function(d, nchunks, create_index_column = FALSE){
    
    nr <- nrow(d)
    chunksize <- nr/nchunks
    
    if(nr > 0){
        
        if(create_index_column) d <- mutate(d, ind = 1:n())
        
        chunklist <- split(d,
                           0:(nr - 1) %/% chunksize)
        
        return(chunklist)
        
    } else {
        
        logwarn(msg = 'Trying to chunk an empty tibble. Something is probably wrong',
                logger = logger_module)
        
        return(d)
    }
}

idw_log_var <- function(verbose,
                        site_code,
                        v,
                        j,
                        nvars,
                        ntimesteps,
                        is_fluxable = NA,
                        note = ''){
    
    if(! verbose) return()
    
    flux_calc_msg <- case_when(is_fluxable == FALSE ~ '[NO FLUX]',
                               is_fluxable == TRUE ~ '[yes flux]',
                               is.na(is_fluxable) ~ '')
    
    note <- ifelse(note == '',
                   note,
                   paste0('[', note, ']'))
    
    msg <- glue::glue('site: {s}; var: {vv} ({jj}/{nv}) {f}; timesteps: {nt}; {n}',
                s = site_code,
                vv = v,
                jj = j,
                nv = nvars,
                nt = ntimesteps,
                f = flux_calc_msg,
                n = note)
    
    print(msg)
    
    #return()
}

ms_parallelize <- function(maxcores = Inf){
    
    #maxcores is the maximum number of processor cores to use for R tasks.
    #   you may want to leave a few aside for other processes.
    
    #value: a cluster object. you'll need this to return to serial mode and
    #   free up the cores that were employed by R. Be sure to run
    #ms_unparallelize() after the parallel tasks are complete.
    
    #we need to find a way to protect some cores for serving the portal
    #if we end up processing data and serving the portal on the same
    #machine/cluster. we can use taskset to assign the shiny process
    #to 1-3 cores and this process to any others.
    
    #NOTE: this function has been updated to work with doFuture, which
    #   supplants doParallel. doFuture allows us to dispatch jobs on a cluster
    #   via Slurm. it might also allow us to dispatch multisession jobs on
    #   Windows
    
    #obsolete-ish notes:
    # #variables used inside the foreach loop on the master process will
    # #be written to the global environment, in some cases overwriting needed
    # #globals. we can set them aside in a separate environment and restore them later
    # protected_vars <- c('prodname_ms', 'verbose', 'site_code')
    # assign(x = 'protected_vars',
    #        val = mget(protected_vars,
    #                   ifnotfound = list(NULL, NULL, NULL),
    #                   inherits = TRUE),
    #        envir = protected_environment)
    
    #then set up parallelization
    # clst <- parallel::makeCluster(ncores)
    clst <- NULL
    ncores <- min(parallel::detectCores(), maxcores)
    
    # Sys.info()[1] %in% c('Linux', 'Darwin')
    
    # if(ms_instance$which_machine == 'DCC'){
        doFuture::registerDoFuture()
        # future::plan(cluster, workers = clst) #might need this instead of Slurm one day
        future::plan(future.batchtools::batchtools_slurm)
    # }
    # } else if(.Platform$OS.type == 'windows'){
    #     #issues (found while testing on linux):
    #     #1. inner precip logging waits till outer loop completes, then only prints to console.
    #     #2. konza error that doesn't occur with FORK cluster:
    #     #   task 1 failed - "task 2 failed - "unused argument (datetime_x = datetime)"
    #     #3. not fully utilizing cores like FORK does
    #     doFuture::registerDoFuture()
    #     # clst <- parallel::makeCluster(ncores)
    #     future::plan(multisession, workers = ncores)
    #     #clst <- parallel::makeCluster(ncores, type = 'PSOCK')
    # } else {
    #     # future::plan(multicore) #can't be done from Rstudio
        clst <- parallel::makeCluster(ncores, type = 'FORK')
        doParallel::registerDoParallel(clst)
    # }
    
    return(clst)
}

idw_parallel_combine <- function(d1, d2){
    
    #this is for use with foreach loops inside the 4 idw prep functions
    #   (precip_idw, pchem_idw, flux_idw, precip_pchem_pflux_idw)
    
    if(is.character(d1) && d1 == 'first iter') return(d2)
    
    d_comb <- bind_rows(d1, d2)
    
    return(d_comb)
}

shortcut_idw <- function(encompassing_dem,
                         wshd_bnd,
                         data_locations,
                         data_values,
                         durations_between_samples = NULL,
                         stream_site_code,
                         output_varname,
                         save_precip_quickref = FALSE,
                         elev_agnostic = FALSE,
                         p_var = NULL,
                         verbose = FALSE,
                         macrosheds_root){
    
    #encompassing_dem: RasterLayer must cover the area of wshd_bnd and precip_gauges
    #wshd_bnd: sf polygon with columns site_code and geometry
    #   it represents a single watershed boundary
    #data_locations:sf point(s) with columns site_code and geometry.
    #   it represents all sites (e.g. rain gauges) that will be used in
    #   the interpolation
    #data_values: data.frame with one column each for datetime and ms_status,
    #   and an additional named column of data values for each data location.
    #durations_between_samples: numeric vector representing the time differences
    #   between rows of data_values. Must be expressed in days. Only used if
    #   output_varname == 'SPECIAL CASE PRECIP' and save_precip_quickref == TRUE,
    #   so that quickref data can be expressed in mm/day, which is the unit
    #   required to calculate precip flux.
    #   NOTE: this could be calculated internally, but the duration of measurement
    #   preceding the first value can't be known. Because shortcut_idw is
    #   often run iteratively on chunks of a dataset, we require that
    #   durations_between_samples be passed as an input, so as to minimize
    #   the number of NAs generated.
    #output_varname: character; a prodname_ms, unless you're interpolating
    #   precipitation, in which case it must be "SPECIAL CASE PRECIP", because
    #   prefix information for precip is lost during the widen-by-site step
    #save_precip_quickref: logical. should interpolated precip for all DEM cells
    #   be saved for later use. Should only be true when precip chem will be
    #   interpolated too. only useable when output_varname = 'PRECIP SPECIAL CASE'
    #elev_agnostic: logical that determines whether elevation should be
    #   included as a predictor of the variable being interpolated
    
    # loginfo(glue('shortcut_idw: working on {ss}', ss=stream_site_code),
    #     logger = logger_module)
    
    if(output_varname != 'SPECIAL CASE PRECIP' && save_precip_quickref){
        stop(paste('save_precip_quickref can only be TRUE if output_varname',
                   '== "SPECIAL CASE PRECIP"'))
    }
    if(save_precip_quickref && is.null(durations_between_samples)){
        stop(paste('save_precip_quickref can only be TRUE if',
                   'durations_between_samples is supplied.'))
    }
    if(output_varname != 'SPECIAL CASE PRECIP' && ! is.null(durations_between_samples)){
        logwarn(msg = paste('In shortcut_idw: ignoring durations_between_samples because',
                            'output_varname != "SPECIAL CASE PRECIP".'),
                logger = logger_module)
    }
    
    if('ind' %in% colnames(data_values)){
        timestep_indices <- data_values$ind
        data_values$ind <- NULL
    }
    
    #matrixify input data so we can use matrix operations
    d_status <- data_values$ms_status
    d_interp <- data_values$ms_interp
    d_dt <- data_values$datetime
    data_matrix <- select(data_values,
                          -ms_status,
                          -datetime,
                          -ms_interp) %>%
        err_df_to_matrix()
    
    #clean dem and get elevation values
    dem_wb <- terra::crop(encompassing_dem, wshd_bnd)
    dem_wb <- terra::mask(dem_wb, wshd_bnd)
    
    wb_is_linear <- terra::nrow(dem_wb) == 1 || terra::ncol(dem_wb) == 1
    wb_all_na <- all(is.na(terra::values(dem_wb)))
    
    if(wb_all_na){
        
        if(wb_is_linear){
            
            #masking will cause trouble here (only known for niwot-MARTINELLI)
            dem_wb <- terra::crop(encompassing_dem, wshd_bnd)
            
        } else {
            stop('some kind of crop/mask issue with small watersheds?')
        }
    }
    
    elevs <- terra::values(dem_wb)
    elevs_masked <- elevs[! is.na(elevs)]
    
    #compute distances from all dem cells to all data locations
    inv_distmat <- matrix(NA,
                          nrow = length(elevs_masked),
                          ncol = ncol(data_matrix),
                          dimnames = list(NULL,
                                          colnames(data_matrix)))
    
    dem_wb_all_na <- dem_wb
    terra::values(dem_wb_all_na) <- NA
    dem_wb_all_na <- terra::rast(dem_wb_all_na)
    for(k in 1:ncol(data_matrix)){
        
        dk <- filter(data_locations,
                     site_code == colnames(data_matrix)[k])
        
        # inv_dists_site <- 1 / raster::distanceFromPoints(dem_wb, dk)^2 %>%
        #     terra::values(.)
        
        inv_dists_site <- 1 / terra::distance(terra::rast(dem_wb_all_na), terra::vect(dk))^2 %>%
            terra::values(.)
        
        # inv_dists_site[is.na(elevs)] <- NA #mask
        inv_dists_site <- inv_dists_site[! is.na(elevs)] #drop elevs not included in mask
        inv_distmat[, k] <- inv_dists_site
    }
    
    # if(output_varname == 'SPECIAL CASE PRECIP'){ REMOVE
    #     precip_quickref <- data.frame(matrix(NA,
    #                                          nrow = ntimesteps,
    #                                          ncol = nrow(inv_distmat)))
    # }
    
    #calculate watershed mean at every timestep
    if(save_precip_quickref) precip_quickref <- list()
    ptm <- proc.time()
    ws_mean <- rep(NA, nrow(data_matrix))
    ntimesteps <- nrow(data_matrix)
    for(k in 1:ntimesteps){
        
        #assign cell weights as normalized inverse squared distances
        dk <- t(data_matrix[k, , drop = FALSE])
        inv_distmat_sub <- inv_distmat[, ! is.na(dk), drop = FALSE]
        dk <- dk[! is.na(dk), , drop = FALSE]
        weightmat <- do.call(rbind, #avoids matrix transposition
                             unlist(apply(inv_distmat_sub, #normalize by row
                                          1,
                                          function(x) list(x / sum(x))),
                                    recursive = FALSE))
        
        #perform vectorized idw
        dk[is.na(dk)] <- 0 #allows matrix multiplication
        d_idw <- weightmat %*% dk
        
        if(nrow(dk) == 0){
            ws_mean[k] <- errors::set_errors(NA_real_, NA)
            if(save_precip_quickref){
                precip_quickref[[k]] <- matrix(NA,
                                               nrow = nrow(d_idw),
                                               ncol = ncol(d_idw))
            }
            next
        }
        
        #reapply uncertainty dropped by `%*%`
        errors::errors(d_idw) <- weightmat %*% matrix(errors::errors(dk),
                                                      nrow = nrow(dk))
        
        #determine data-elevation relationship for interp weighting
        if(! elev_agnostic && nrow(dk) >= 3){
            d_elev <- tibble(site_code = rownames(dk),
                             d = dk[,1]) %>%
                left_join(data_locations,
                          by = 'site_code')
            mod <- lm(d ~ elevation, data = d_elev)
            ab <- as.list(mod$coefficients)
            
            #estimate raster values from elevation alone
            d_from_elev <- ab$elevation * elevs_masked + ab$`(Intercept)`
            
            # Set all negative values to 0
            d_from_elev[d_from_elev < 0] <- 0
            
            #average both approaches (this should be weighted toward idw
            #when close to any data location, and weighted half and half when far)
            # d_idw <- mapply(function(x, y) mean(c(x, y), na.rm=TRUE),
            #                 d_idw,
            #                 d_from_elev)
            d_idw <- (d_idw + d_from_elev) / 2
        }
        
        ws_mean[k] <- mean(d_idw, na.rm=TRUE)
        errors::errors(ws_mean)[k] <- mean(errors::errors(d_idw), na.rm=TRUE)
        
        # quickref_ind <- k %% 1000 REMOVE
        # update_quickref <- quickref_ind == 0
        # if(! update_quickref){
        # precip_quickref[[quickref_ind]] <- p_idw
        if(save_precip_quickref) precip_quickref[[k]] <- d_idw
        # } else {
        
        # precip_quickref[[1000]] <- p_idw
        
        # save_precip_quickref(precip_idw_list = precip_quickref,
        #                      network = network,
        #                      domain = domain,
        #                      site_code = stream_site_code,
        #                      # chunk_number = quickref_chunk)
        #                      timestep = k)
        # }
    }
    
    if(save_precip_quickref){
        
        #convert to mm/d
        precip_quickref <- base::Map(
            f = function(millimeters, days){
                return(millimeters/days)
            },
            millimeters = precip_quickref,
            days = durations_between_samples)
        
        names(precip_quickref) <- as.character(timestep_indices)
        write_precip_quickref(precip_idw_list = precip_quickref,
                              site_code = stream_site_code,
                              chunkdtrange = range(d_dt),
                              macrosheds_root = macrosheds_root)
    }
    # compare_interp_methods()
    
    if(output_varname == 'SPECIAL CASE PRECIP'){
        
        ws_mean <- tibble(datetime = d_dt,
                          site_code = stream_site_code,
                          var = p_var,
                          val = ws_mean,
                          ms_status = d_status,
                          ms_interp = d_interp)

    } else {
        
        ws_mean <- tibble(datetime = d_dt,
                          site_code = stream_site_code,
                          var = output_varname,
                          concentration = ws_mean,
                          ms_status = d_status,
                          ms_interp = d_interp)
    }
    
    return(ws_mean)
}

ms_unparallelize <- function(cluster_object){
    
    #if cluster_object is NULL, nothing will happen
    
    # tryCatch({print(site_code)},
    #         error=function(e) print('nope'))
    
    if(is.null(cluster_object)){
        future::plan(future::sequential)
        return()
    }
    
    parallel::stopCluster(cluster_object)
    
    #remove foreach clutter that might compromise the next parallel run
    fe_junk <- foreach:::.foreachGlobals
    
    rm(list = ls(name = fe_junk),
       pos = fe_junk)
    
    # #remove any unneeded globals that were created during parallelization
    # unneeded_globals <- c('pchem_vars', 'pchem_vars_fluxable',
    #                       'dem', 'wbi', 'rg', 'precip',
    #                       'pchem_setlist', 'first_fluxvar_ind', 'i', 'j')
    # sw(rm(list = unneeded_globals,
    #       envir = .GlobalEnv))
    
    # #restore globals that were overwritten during parallelization
    # protected_vars <- mget('protected_vars',
    #                           envir = protected_environment)
    #
    # for(i in 1:length(protected_vars)){
    #
    #     nm <- names(protected_vars)[i]
    #     val <- protected_vars[[i]]
    #
    #     if(! is.null(val)){
    #         assign(nm,
    #                value = val,
    #                envir = .GlobalEnv)
    #     } else {
    #
    #         #or remove them if they didn't exist before parallelization
    #         sw(rm(list = nm,
    #               envir = .GlobalEnv))
    #     }
    # }
}

apply_detection_limit_t <- function(X,
                                    network,
                                    domain,
                                    prodname_ms,
                                    ignore_pred = FALSE){
    
    #this is the temporally explicit version of apply_detection_limit (_t).
    #it supersedes the scalar version (apply_detection_limit_s).
    #that version just returns its output. This version relies on stored data,
    #so automatically reads from data/<network>/<domain>/detection_limits.json.
    
    #X is a 2d array-like object. must have datetime,
    #   site_code, var, and val columns. if X was generated by ms_cast_and_reflag,
    #   you should be good to go.
    #ignore_pred: logical; set to TRUE if detection limits should be retrieved
    #   from the supplied prodname_ms directly, rather than its precursor.
    
    #Attempting to apply detection
    #limits to a variable for which detection limits are not known (not present
    #in detection_limits.json) results in error. Superfluous variable entries in
    #detection_limits.json are ignored.
    
    X <- as_tibble(X) %>%
        arrange(site_code, var, datetime)
    
    if(ignore_pred &&
       (length(prodname_ms) > 1 || ! is_derived_product(prodname_ms))){
        stop('If ignoring precursors, a single derived prodname_ms must be supplied')
    }
    
    if(length(prodname_ms) > 1 && any(is_derived_product(prodname_ms))){
        #i can't think of a time when we'd need to supply multiple derived
        #products and knit them, but if such a case exists feel free to
        #amend this error checker
        stop('Cannot knit multiple detlims when one or more of them is derived.')
    }
    
    if(length(prodname_ms) == 1 &&
       is_derived_product(prodname_ms) &&
       ! ignore_pred){
        
        prodname_ms <- get_detlim_precursors(network = network,
                                             domain = domain,
                                             prodname_ms = prodname_ms)
    }
    
    if(length(prodname_ms) > 1) {
        detlim <- knit_det_limits(network = network,
                                  domain = domain,
                                  prodname_ms = prodname_ms)
    } else {
        detlim <- read_detection_limit(network = network,
                                       domain = domain,
                                       prodname_ms = prodname_ms)
    }
    
    if(is_ms_err(detlim)){
        stop('problem reading detection limits from file')
    }
    
    apply_detection_limit_ <- function(x, varnm, detlim){
        
        
        #plenty of code superfluity in this function. adapted from a previous
        #   version and there's negligible efficiency loss if any
        
        if(! varnm %in% names(detlim)){
            stop(glue::glue('Missing detection limits for var: {v}', v = varnm))
        }
        
        detlim_var <- detlim[[varnm]]
        
        x <- filter(x, var == varnm)
        
        #nrow(x) == 1 was added because there was an error occurring if there
        #was a site with only one sample of a variable
        if(nrow(x) == 0){
            return(NULL)
        }
        
        sn = x$site_code
        dt = x$datetime
        
        site_lst <- tibble(dt, sn, val = x$val) %>%
            base::split(sn)
        
        Xerr <- lapply(X = site_lst,
                       FUN = function(z) errors(z$val)) %>%
            unlist() %>%
            unname()
        
        rounded <- lapply(X = site_lst,
                          FUN = function(z){
                              
                              if(all(is.na(z$val))) return(z$val)
                              
                              detlim_varsite <- detlim_var[[z$sn[1]]]
                              if(all(is.na(detlim_varsite$lim))) return(z$val)
                              
                              cutvec <- c(as.POSIXct(detlim_varsite$startdt,
                                                     tz = 'UTC'),
                                          as.POSIXct('2900-01-01 00:00:00'))
                              
                              roundvec <- cut(x = z$dt,
                                              breaks = cutvec,
                                              include.lowest = TRUE,
                                              labels = detlim_varsite$lim) %>%
                                  as.character() %>%
                                  as.numeric()
                              
                              #sometimes synchronize_timestep will adjust a point
                              #to a time before the earliest startdt recorded
                              #in detection_limits.json. this handles that.
                              if(length(roundvec == 1) && is.na(roundvec)){
                                  roundvec = detlim_varsite$lim[1]
                              } else {
                                  roundvec <- imputeTS::na_locf(x = roundvec,
                                                                option = 'nocb')
                              }
                              
                              rounded <- mapply(FUN = function(a, b){
                                  round(a, b)
                              },
                              a = z$val,
                              b = roundvec,
                              USE.NAMES = FALSE)
                              
                              return(rounded)
                          }) %>%
            unlist() %>%
            unname()
        
        errors(rounded) <- Xerr
        
        return(rounded)
    }
    
    variables <- unique(X$var)
    
    for(v in variables){
        for(s in unique(X$site_code)){
            x <- filter(X, site_code == s)
            dlv <- apply_detection_limit_(x, v, detlim)
            if(is.null(dlv)) next
            X$val[X$site_code == s & X$var == v] <- dlv
        }
    }
    
    return(X)
}

write_ms_file <- function(d,
                          prodname_ms,
                          site_code,
                          shapefile = FALSE,
                          sep_errors = TRUE,
                          macrosheds_root){
    
    #write an ms tibble or shapefile to its appropriate destination based on
    #network, domain, prodname_ms, site_code, and processing level. If a tibble,
    #write as a feather file (site_code.feather). Uncertainty (error) associated
    #with the val column will be extracted into a separate column called
    #val_err. Write the file to the appropriate location within the data
    #acquisition repository.
    
    #deprecated:
    #if link_to_portal == TRUE, create a hard link to the
    #file from the portal repository, which is assumed to be a sibling of the
    #data_acquision directory and to be named "portal".
    
    if(shapefile){
        
        site_dir = glue::glue('{ms}/{p}/{s}',
                        ms = macrosheds_root,
                        p = prodname_ms,
                        s = site_code)
        
        dir.create(site_dir,
                   showWarnings = FALSE,
                   recursive = TRUE)
        
        if(any(! sf::st_is_valid(d))) {
            d <- sf::st_make_valid(d)
        }
        
        sw(sf::st_write(obj = d,
                        dsn = glue::glue(site_dir, '/', site_code, '.shp'),
                        delete_dsn = TRUE,
                        quiet = TRUE))
        
    } else {
        
        prod_dir = glue::glue('{ms}/{p}',
                        ms = macrosheds_root,
                        p = prodname_ms)
        
        dir.create(prod_dir,
                   showWarnings = FALSE,
                   recursive = TRUE)
        
        site_file = glue::glue('{pd}/{s}.feather',
                         pd = prod_dir,
                         s = site_code)
        
        if(sep_errors) {
            
            #separate uncertainty into a new column.
            #remove errors attribute from val column if it exists (it always should)
            d$val_err <- errors::errors(d$val)
            if('errors' %in% class(d$val)){
                d$val <- errors::drop_errors(d$val)
            } else {
                warning(glue::glue('Uncertainty missing from val column ({n}-{d}-{s}-{p}). ',
                             'That means this dataset has not passed through ',
                             'carry_uncertainty yet. it should have.',
                             n = network,
                             d = domain,
                             s = site_code,
                             p = prodname_ms))
            }
        }
        #make sure write_feather will omit attrib by def (with no artifacts)
        feather::write_feather(d, site_file)
    }
    
    #return()
}

shortcut_idw_concflux_v2 <- function(encompassing_dem,
                                     wshd_bnd,
                                     ws_area,
                                     data_locations,
                                     precip_values,
                                     chem_values,
                                     stream_site_code,
                                     output_varname,
                                     out_path,
                                     # dump_idw_precip,
                                     verbose = FALSE){
    
    #This replaces shortcut_idw_concflux! shortcut_idw is still used for
    #variables that can't be flux-converted, and for precipitation
    
    #this function is similar to shortcut_idw_concflux.
    #if the variable represented by chem_values and output_varname is
    #flux-convertible, it multiplies precip chem
    #by precip volume to calculate flux for each cell and returns
    #the means of IDW-interpolated precipitation, precip chem, and precip flux
    #for each sample timepoint. If that variable is not flux-convertible,
    #It only returns precipitation and precip chem. All interpolated products are
    #returned as standard macrosheds timeseries tibbles in a single list.
    
    #encompassing_dem: RasterLayer; must cover the area of wshd_bnd and
    #   recip_gauges
    #wshd_bnd: sf polygon with columns site_code and geometry.
    #   it represents a single watershed boundary.
    #ws_area: numeric scalar representing watershed area in hectares. This is
    #   passed so that it doesn't have to be calculated repeatedly (if
    #   shortcut_idw_concflux_v2 is running iteratively).
    #data_locations: sf point(s) with columns site_code and geometry.
    #   represents all sites (e.g. rain gauges) that will be used in
    #   the interpolation.
    #precip_values: a data.frame with datetime, ms_status, ms_interp,
    #   and a column of data values for each precip location.
    #chem_values: a data.frame with datetime, ms_status, ms_interp,
    #   and a column of data values for each precip chemistry location.
    #stream_site_code: character; the name of the watershed/stream, not the
    #   name of a precip gauge
    #output_varname: character; the prodname_ms used to populate the var
    #   column in the returned tibble
    #dump_idw_precip: logical; if TRUE, IDW-interpolated precipitation will
    #   be dumped to disk (data/<network>/<domain>/precip_idw_dumps/<site_code>.rds).
    #   this file will then be read by precip_pchem_pflux_idw, in order to
    #   properly build data/<network>/<domain>/derived/<precipitation_msXXX>.
    #   the precip_idw_dumps directory is automatically removed after it's used.
    #   This should only be set to TRUE for one iteration of the calling loop,
    #   or else time will be wasted rewriting the files.
    #   REMOVED; OBSOLETE
    
    precip_quickref <- read_precip_quickref(out_path = out_path,
                                            site_code = stream_site_code,
                                            dtrange = range(chem_values$datetime))
    
    if(length(precip_quickref) == 1){
        
        just_checkin <- precip_quickref[[1]]
        
        if(class(just_checkin) == 'character' &&
           just_checkin == 'NO QUICKREF AVAILABLE'){
            
            return(tibble())
        }
    }
    
    precip_is_highres <- Mode(diff(as.numeric(precip_values$datetime))) <= 15 * 60
    if(is.na(precip_is_highres)) precip_is_highres <- FALSE
    chem_is_highres <- Mode(diff(as.numeric(chem_values$datetime))) <= 15 * 60
    if(is.na(chem_is_highres)) chem_is_highres <- FALSE
    
    #if both chem and precip data are low resolution (grab samples),
    #   let approxjoin_datetime match up samples with a 12-hour gap. otherwise the
    #   gap should be 7.5 mins so that there isn't enormous duplication of
    #   timestamps where multiple high-res values can be snapped to the
    #   same low-res value
    if(! chem_is_highres && ! precip_is_highres){
        join_distance <- c('12:00:00')
    } else {
        join_distance <- c('7:30')
    }
    
    dt_match_inds <- approxjoin_datetime(x = chem_values,
                                         y = precip_values,
                                         rollmax = join_distance,
                                         indices_only = TRUE)
    
    chem_values <- chem_values[dt_match_inds$x, ]
    common_datetimes <- chem_values$datetime
    
    if(length(common_datetimes) == 0){
        pchem_range <- range(chem_values$datetime)
        test <- filter(precip_values,
                       datetime > pchem_range[1],
                       datetime < pchem_range[2])
        if(nrow(test) > 0){
            logging::logerror('something is wrong with approxjoin_datetime')
        }
        return(tibble())
    }
    
    precip_values <- precip_values %>%
        mutate(ind = 1:n()) %>%
        slice(dt_match_inds$y) %>%
        mutate(datetime = !!common_datetimes)
    
    quickref_inds <- precip_values$ind
    precip_values$ind <- NULL
    
    #matrixify input data so we can use matrix operations
    p_status <- precip_values$ms_status
    p_interp <- precip_values$ms_interp
    p_matrix <- select(precip_values,
                       -ms_status,
                       -datetime,
                       -ms_interp) %>%
        err_df_to_matrix()
    
    c_status <- chem_values$ms_status
    c_interp <- chem_values$ms_interp
    c_matrix <- select(chem_values,
                       -ms_status,
                       -datetime,
                       -ms_interp) %>%
        err_df_to_matrix()
    
    rm(precip_values, chem_values); gc()
    
    d_status <- bitwOr(p_status, c_status)
    d_interp <- bitwOr(p_interp, c_interp)
    
    #clean dem and get elevation values
    dem_wb <- terra::crop(encompassing_dem, wshd_bnd)
    dem_wb <- terra::mask(dem_wb, wshd_bnd)
    elevs <- terra::values(dem_wb)
    elevs_masked <- elevs[! is.na(elevs)]
    
    #compute distances from all dem cells to all chemistry locations
    inv_distmat_c <- matrix(NA,
                            nrow = length(elevs_masked),
                            ncol = ncol(c_matrix), #ngauges
                            dimnames = list(NULL,
                                            colnames(c_matrix)))
    
    dem_wb_all_na <- dem_wb
    terra::values(dem_wb_all_na) <- NA
    dem_wb_all_na <- terra::rast(dem_wb_all_na)
    for(k in 1:ncol(c_matrix)){
        dk <- filter(data_locations,
                     site_code == colnames(c_matrix)[k])
        
        inv_dists_site <- 1 / terra::distance(dem_wb_all_na, terra::vect(dk))^2 %>%
            terra::values(.)
        inv_dists_site <- inv_dists_site[! is.na(elevs)] #drop elevs not included in mask
        inv_distmat_c[, k] <- inv_dists_site
    }
    
    #calculate watershed mean concentration and flux at every timestep
    ptm <- proc.time()
    ntimesteps <- nrow(c_matrix)
    ws_mean_conc <- ws_mean_flux <- rep(NA, ntimesteps)
    
    for(k in 1:ntimesteps){
        
        ## GET CHEMISTRY FOR ALL CELLS IN TIMESTEP k
        
        #assign cell weights as normalized inverse squared distances (c)
        ck <- t(c_matrix[k, , drop = FALSE])
        inv_distmat_c_sub <- inv_distmat_c[, ! is.na(ck), drop=FALSE]
        ck <- ck[! is.na(ck), , drop=FALSE]
        weightmat_c <- do.call(rbind,
                               unlist(apply(inv_distmat_c_sub,
                                            1,
                                            function(x) list(x / sum(x))),
                                      recursive = FALSE))
        
        if(ncol(weightmat_c) == 0){
            ws_mean_conc[k] <- NA_real_ %>% errors::set_errors(NA)
            ws_mean_flux[k] <- NA_real_ %>% errors::set_errors(NA)
            next
        }
        
        #perform vectorized idw (c)
        ck[is.na(ck)] <- 0
        c_idw <- weightmat_c %*% ck
        
        #reapply uncertainty dropped by `%*%`
        errors::errors(c_idw) <- weightmat_c %*% matrix(errors::errors(ck),
                                                        nrow = nrow(ck))
        
        ## GET FLUX FOR ALL CELLS; THEN AVERAGE CELLS FOR PCHEM, PFLUX, PRECIP
        
        #calculate flux for every cell:
        #   This is how we'd calcualate flux as kg/(ha * d):
        #       mm/d * mg/L * m/1000mm * kg/1,000,000mg * 1000L/m^(2 + 1) * 10,000m^2/ha
        #       therefore, kg/(ha * d) = mm * mg/L / d / 100 = (mm * mg) / (d * L * 100)
        #   But stream_flux_inst is not scaled by area when it's derived (that
        #   happens later), so we're going to derive precip_flux_inst
        #   as unscaled here, and then both can be scaled the same way in
        #   scale_flux_by_area. So we calculate flux in kg/(ha * d) as above,
        #   then multiply by watershed area in hectares.
        
        quickref_ind <- as.character(quickref_inds[k])
        #              mg/L        mm/day                          ha
        flux_interp <- c_idw * precip_quickref[[quickref_ind]] * ws_area / 100
        
        #calculate watershed averages (work around error drop)
        ws_mean_conc[k] <- mean(c_idw, na.rm=TRUE)
        ws_mean_flux[k] <- mean(flux_interp, na.rm=TRUE)
        errors::errors(ws_mean_conc)[k] <- mean(errors::errors(c_idw), na.rm=TRUE)
        errors::errors(ws_mean_flux)[k] <- mean(errors::errors(flux_interp), na.rm=TRUE)
    }
    
    # compare_interp_methods()
    
    ws_means <- tibble(datetime = common_datetimes,
                       site_code = stream_site_code,
                       var = output_varname,
                       concentration = ws_mean_conc,
                       flux = ws_mean_flux,
                       ms_status = d_status,
                       ms_interp = d_interp)
    
    return(ws_means)
}

expo_backoff <- function(expr,
                         max_attempts = 10,
                         verbose = TRUE){
    
    for(attempt_i in seq_len(max_attempts)){
        
        results <- try(expr = expr,
                       silent = TRUE)
        
        if(inherits(results, 'try-error')){
            
            if(attempt_i == max_attempts){
                stop(attr(results, 'condition'))
            }
            
            backoff <- runif(n = 1,
                             min = 0,
                             max = 2^attempt_i - 1)
            
            if(verbose){
                print(glue::glue("Backing off for ", round(backoff, 1), " seconds."))
            }
            
            Sys.sleep(backoff)
            
        } else {
            
            # if(verbose){
            #     print(paste0("Request succeeded after ", attempt_i, " attempt(s)."))
            # }
            
            break
        }
    }
    
    return(results)
}

idw_log_wb <- function(verbose, site_code, i, nw){
    
    if(! verbose) return()
    
    msg <- glue::glue('site: {s} ({ii}/{w})',
                      s = site_code,
                      ii = i,
                      w = nw)
    
    print(msg)
    
    #return()
}

knit_det_limits <- function(network, domain, prodname_ms){
    
    
    detlim <- read_detection_limit(network, domain, prodname_ms[1])
    
    if(is.null(detlim)){
        prodname_ms <- get_successor(network = network,
                                     domain = domain,
                                     prodname_ms = prodname_ms[1])
    }
    
    for(i in 2:length(prodname_ms)) {
        detlim_ <- read_detection_limit(network, domain, prodname_ms[i])
        
        old_vars <- names(detlim)
        new_vars <- names(detlim_)
        
        common_vars <- base::intersect(old_vars, new_vars)
        
        if(length(common_vars) > 0) {
            
            for(p in 1:length(common_vars)) {
                
                old_sites <- names(detlim[[common_vars[p]]])
                new_sites <- names(detlim_[[common_vars[p]]])
                
                common_sites <- base::intersect(old_sites, new_sites)
                
                if(!length(common_sites) == 0) {
                    new_sites <- new_sites[!new_sites %in% common_sites]
                }
                
                if(length(new_sites) > 0){
                    for(z in 1:length(new_sites)) {
                        detlim[[common_vars[p]]][[new_sites[z]]] <- detlim_[[common_vars[p]]][[new_sites[z]]]
                    }
                }
            }
        }
        
        unique_vars <- new_vars[!new_vars %in% old_vars]
        if(length(unique_vars) > 0) {
            for(v in 1:length(unique_vars)) {
                detlim[[unique_vars[v]]] <- detlim_[[unique_vars[v]]]
            }
            
        }
    }
    
    return(detlim)
}

err_df_to_matrix <- function(df){
    
    if(! all(sapply(df, class) %in% c('errors', 'numeric'))){
        stop('all columns of df must be of class "errors" or "numeric"')
    }
    
    errmat <- as.matrix(as.data.frame(lapply(df, errors::errors)))
    M <- as.matrix(df)
    errors::errors(M) <- errmat
    
    return(M)
}

write_precip_quickref <- function(precip_idw_list,
                                  macrosheds_root,
                                  site_code,
                                  chunkdtrange){
    # timestep){
    
    #allows precip values computed by shortcut_idw for each watershed
    #   raster cell to be reused by shortcut_idw_concflux_v2
    
    quickref_dir <- glue::glue('{mr}/precip_idw_quickref/',
                               mr = macrosheds_root)
    
    dir.create(path = quickref_dir,
               showWarnings = FALSE,
               recursive = TRUE)
    
    chunkfile <- paste(strftime(chunkdtrange[1],
                                format = '%Y-%m-%d %H:%M:%S',
                                tz = 'UTC'),
                       strftime(chunkdtrange[2],
                                format = '%Y-%m-%d %H:%M:%S',
                                tz = 'UTC'),
                       sep = '_')
    
    chunkfile <- str_replace_all(chunkfile, ':', '-')
    
    saveRDS(object = precip_idw_list,
            file = glue::glue('{qd}/{cf}', #omitting extension for easier parsing
                              qd = quickref_dir,
                              cf = chunkfile))
    
    #previous approach: when chunks have a size limit:
    
    # #not to be confused with the precip idw tempfile (dumpfile),
    # #the quickref file allows the same precip idw data to be used across all
    # #chemistry variables when calculating flux. it's stored in 1000-timestep
    # #chunks
    #
    # chunk_number <- floor((timestep - 1) / 1000) + 1
    # chunkID <- stringr::str_pad(string = chunk_number,
    #                             width = 3,
    #                             side = 'left',
    #                             pad = '0')
    #
    # quickref_dir <- glue('data/{n}/{d}/precip_idw_quickref/{s}',
    #                      n = network,
    #                      d = domain,
    #                      s = site_code)
    #
    # chunkfile <- glue('chunk{ch}.rds',
    #                   ch = chunkID)
    #
    # if(! file.exists(chunkfile)){ #in case another thread has already written it
    #
    #     dir.create(path = quickref_dir,
    #                showWarnings = FALSE,
    #                recursive = TRUE)
    #
    #     saveRDS(object = precip_idw_list,
    #             file = paste(quickref_dir,
    #                          chunkfile,
    #                          sep = '/'))
    # }
}

reconstruct_var_column <- function(d,
                                   network,
                                   domain,
                                   prodname,
                                   level = 'munged'){
    
    #currently only used inside the precip idw interpolator, where
    #we no longer have variable prefix information. this attempts
    #to determine that information from the stored detlim file.
    #it's not yet equipped to handle the case where precipitation
    #(or any other variable) has multiple prefixes through time.
    
    #returns d with a new var column
    
    if(! level %in% c('munged', 'derived')){
        stop('level must be either "munged" or "derived"')
    }
    
    prods <- sm(read_csv(glue::glue('src/{n}/{d}/products.csv',
                              n = network,
                              d = domain)))
    
    rgx <- ifelse(level == 'munged',
                  '^(?!ms[0-9]{3}).*?',
                  '^ms[0-9]{3}$')
    
    prodname_ms <- prods %>%
        filter(prodname == !!prodname,
               grepl(pattern = rgx,
                     x = prodcode,
                     perl = TRUE)) %>%
        mutate(prodname_ms = paste(prodname,
                                   prodcode,
                                   sep = '__')) %>%
        pull(prodname_ms)
    
    detlim <- tryCatch(
        {
            if(length(prodname_ms) > 1){
                knit_det_limits(network = network,
                                domain = domain,
                                prodname_ms = prodname_ms)
            } else {
                read_detection_limit(network = network,
                                     domain = domain,
                                     prodname_ms = prodname_ms)
            }
        },
        error = function(e){
            stop(glue::glue('could not read detection limits, which are ',
                      'needed for reconstructing the var column'))
        }
    )
    
    if(length(detlim) == 1){
        var <- names(detlim)
    } else {
        # # d <<- d
        # vv <<- detlim
        # detlim = vv
        stop(glue::glue('Not sure if we\'ll ever encounter this, but if ',
                  'so we need to build it now!'))
    }
    
    d <- d %>%
        mutate(var = !!var) %>%
        select(datetime, site_code, var,
               any_of(x = c('val', 'concentration', 'flux')),
               ms_status, ms_interp)
    
    return(d)
}

choose_projection <- function(lat = NULL,
                              long = NULL,
                              unprojected = FALSE){
    
    #TODO: CHOOSE PROJECTIONS MORE CAREFULLY
    
    if(unprojected){
        PROJ4 <- glue::glue('+proj=longlat +datum=WGS84 +no_defs ',
                            '+ellps=WGS84 +towgs84=0,0,0')
        return(PROJ4)
    }
    
    if(is.null(lat) || is.null(long)){
        stop('If projecting, lat and long are required.')
    }
    
    abslat <- abs(lat)
    
    # if(abslat < 23){ #tropical
    #     PROJ4 = glue('+proj=laea +lon_0=', long)
    #              # ' +datum=WGS84 +units=m +no_defs')
    # } else { #temperate or polar
    #     PROJ4 = glue('+proj=laea +lat_0=', lat, ' +lon_0=', long)
    # }
    
    #this is what the makers of https://projectionwizard.org/# use to choose
    #a suitable projection: https://rdrr.io/cran/rCAT/man/simProjWiz.html
    # THIS WORKS (PROJECTS STUFF), BUT CAN'T BE READ AUTOMATICALLY BY st_read
    if(abslat < 70){ #tropical or temperate
        PROJ4 <- glue::glue('+proj=cea +lon_0={lng} +lat_ts=0 +x_0=0 +y_0=0 ',
                            '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                            lng = long)
    } else { #polar
        PROJ4 <- glue::glue('+proj=laea +lat_0={lt} +lon_0={lng} +x_0=0 +y_0=0 ',
                            '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                            lt = lat,
                            lng = long)
    }
    
    ## UTM/UPS would be nice for watersheds that don't fall on more than two zones
    ## (incomplete)
    # if(lat > 84 || lat < -80){ #polar; use Universal Polar Stereographic (UPS)
    #     PROJ4 <- glue('+proj=ups +lon_0=', long)
    #              # ' +datum=WGS84 +units=m +no_defs')
    # } else { #not polar; use UTM
    #     PROJ4 <- glue('+proj=utm +lat_0=', lat, ' +lon_0=', long)
    # }
    
    ## EXTRA CODE FOR CHOOSING PROJECTION BY LATITUDE ONLY
    # if(abslat < 23){ #tropical
    #     PROJ4 <- 9835 #Lambert cylindrical equal area (ellipsoidal; should spherical 9834 be used instead?)
    # } else if(abslat > 23 && abslat < 66){ # middle latitudes
    #     PROJ4 <- 5070 #albers equal area conic
    # } else { #polar (abslat >= 66)
    #     PROJ4 <- 9820 #lambert equal area azimuthal
    #     # PROJ4 <- 1027 #lambert equal area azimuthal (spherical)
    # }
    # PROJ4 <- 3857 #WGS 84 / Pseudo-Mercator
    # PROJ4 <- 2163
    
    return(PROJ4)
}

read_precip_quickref <- function(out_path,
                                 site_code,
                                 dtrange){
    # timestep){
    
    #allows precip values computed by shortcut_idw for each watershed
    #   raster cell to be reused by shortcut_idw_concflux_v2. These values are
    #   in mm/day
    
    quickref_dir <- glue::glue('{ms}/precip_idw_quickref',
                               ms = out_path)
    
    quickref_chunks <- list.files(quickref_dir)
    
    refranges <- lapply(quickref_chunks,
                        function(x){
                            as.POSIXct(strsplit(x, '_')[[1]],
                                       tz = 'UTC')
                        }) %>%
        plyr::ldply(function(y){
            data.frame(startdt = y[1],
                       enddt = y[2])
        }) %>%
        mutate(ref_ind = 1:n())
    
    refranges_sel <- refranges %>%
        filter((startdt >= dtrange[1] & enddt <= dtrange[2]) |
                   (startdt < dtrange[1] & enddt >= dtrange[1]) |
                   (enddt > dtrange[2] & startdt <= dtrange[2]))
    #redundant?
    # (startdt > dtrange[1] & startdt <= dtrange[2] & enddt > dtrange[2]) |
    # (startdt < dtrange[1] & enddt < dtrange[2] & enddt >= dtrange[1]))
    
    if(nrow(refranges_sel) == 0){
        return(list('0' = 'NO QUICKREF AVAILABLE'))
    }
    
    #handle the case where an end of dtrange falls right between the start and
    #end dates of a quickref file. this is possible because precip dates can
    #be shifted (replaced with pchem dates) inside precip_pchem_pflux_idw2
    ref_ind_range <- range(refranges_sel$ref_ind)
    
    if(dtrange[1] < refranges_sel$startdt[1] && ref_ind_range[1] > 1){
        
        refranges_sel <- bind_rows(refranges[ref_ind_range[1] - 1, ],
                                   refranges_sel)
    }
    
    if(dtrange[2] > refranges_sel$enddt[nrow(refranges_sel)] &&
       ref_ind_range[2] < nrow(refranges)){
        
        refranges_sel <- bind_rows(refranges_sel,
                                   refranges[ref_ind_range[2] + 1, ])
    }
    
    quickref <- list()
    for(i in 1:nrow(refranges_sel)){
        
        fn <- paste(strftime(refranges_sel$startdt[i],
                             format = '%Y-%m-%d %H-%M-%S',
                             tz = 'UTC'),
                    strftime(refranges_sel$enddt[i],
                             format = '%Y-%m-%d %H-%M-%S',
                             tz = 'UTC'),
                    sep = '_')
        
        qf <- readRDS(glue::glue('{qd}/{f}',
                                 qd = quickref_dir,
                                 f = fn))
        
        quickref <- append(quickref, qf)
    }
    
    #for some reason the first ref sometimes gets duplicated?
    quickref <- quickref[! duplicated(names(quickref))]
    
    return(quickref)
}

approxjoin_datetime <- function(x,
                                y,
                                rollmax = '7:30',
                                keep_datetimes_from = 'x',
                                indices_only = FALSE){
    #direction = 'forward'){
    
    #x and y: macrosheds standard tibbles with only one site_code,
    #   which must be the same in x and y. Nonstandard tibbles may also work,
    #   so long as they have datetime columns, but the only case where we need
    #   this for other tibbles is inside precip_pchem_pflux_idw, in which case
    #   indices_only == TRUE, so it's not really set up for general-purpose joining
    #rollmax: the maximum snap time for matching elements of x and y.
    #   either '7:30' for continuous data or '12:00:00' for grab data
    #direction [REMOVED]: either 'forward', meaning elements of x will be rolled forward
    #   in time to match the next y, or 'backward', meaning elements of
    #   x will be rolled back in time to reach the previous y
    #keep_datetimes_from: string. either 'x' or 'y'. the datetime column from
    #   the corresponding tibble will be kept, and the other will be dropped
    #indices_only: logical. if TRUE, a join is not performed. rather,
    #   the matching indices from each tibble are returned as a named list of vectors..
    
    #good datasets for testing this function:
    # x <- tribble(
    #     ~datetime, ~site_code, ~var, ~val, ~ms_status, ~ms_interp,
    #     '1968-10-09 04:42:00', 'GSWS10', 'GN_alk', set_errors(27.75, 1), 0, 0,
    #     '1968-10-09 04:44:00', 'GSWS10', 'GN_alk', set_errors(21.29, 1), 0, 0,
    #     '1968-10-09 04:47:00', 'GSWS10', 'GN_alk', set_errors(21.29, 1), 0, 0,
    #     '1968-10-09 04:59:59', 'GSWS10', 'GN_alk', set_errors(16.04, 1), 0, 0,
    #     '1968-10-09 05:15:01', 'GSWS10', 'GN_alk', set_errors(17.21, 1), 1, 0,
    #     '1968-10-09 05:30:59', 'GSWS10', 'GN_alk', set_errors(16.50, 1), 0, 0) %>%
    # mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
    # y <- tribble(
    #     ~datetime, ~site_code, ~var, ~val, ~ms_status, ~ms_interp,
    #     '1968-10-09 04:00:00', 'GSWS10', 'GN_alk', set_errors(1.009, 1), 1, 0,
    #     '1968-10-09 04:15:00', 'GSWS10', 'GN_alk', set_errors(2.009, 1), 1, 1,
    #     '1968-10-09 04:30:00', 'GSWS10', 'GN_alk', set_errors(3.009, 1), 1, 1,
    #     '1968-10-09 04:45:00', 'GSWS10', 'GN_alk', set_errors(4.009, 1), 1, 1,
    #     '1968-10-09 05:00:00', 'GSWS10', 'GN_alk', set_errors(5.009, 1), 1, 1,
    #     '1968-10-09 05:15:00', 'GSWS10', 'GN_alk', set_errors(6.009, 1), 1, 1) %>%
    #     mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
    
    #tests
    if('site_code' %in% colnames(x) && length(unique(x$site_code)) > 1){
        stop('Only one site_code allowed in x at the moment')
    }
    if('var' %in% colnames(x) && length(unique(drop_var_prefix(x$var))) > 1){
        stop('Only one var allowed in x at the moment (not including prefix)')
    }
    if('site_code' %in% colnames(y) && length(unique(y$site_code)) > 1){
        stop('Only one site_code allowed in y at the moment')
    }
    if('var' %in% colnames(y) && length(unique(drop_var_prefix(y$var))) > 1){
        stop('Only one var allowed in y at the moment (not including prefix)')
    }
    if('site_code' %in% colnames(x) &&
       'site_code' %in% colnames(y) &&
       x$site_code[1] != y$site_code[1]) stop('x and y site_code must be the same')
    if(! rollmax %in% c('7:30', '12:00:00')) stop('rollmax must be "7:30" or "12:00:00"')
    # if(! direction %in% c('forward', 'backward')) stop('direction must be "forward" or "backward"')
    if(! keep_datetimes_from %in% c('x', 'y')) stop('keep_datetimes_from must be "x" or "y"')
    if(! 'datetime' %in% colnames(x) || ! 'datetime' %in% colnames(y)){
        stop('both x and y must have "datetime" columns containing POSIXct values')
    }
    if(! is.logical(indices_only)) stop('indices_only must be a logical')
    
    #deal with the case of x or y being a specialized "flow" tibble
    # x_is_flowtibble <- y_is_flowtibble <- FALSE
    # if('flow' %in% colnames(x)) x_is_flowtibble <- TRUE
    # if('flow' %in% colnames(y)) y_is_flowtibble <- TRUE
    # if(x_is_flowtibble && ! y_is_flowtibble){
    #     varname <- y$var[1]
    #     y$var = NULL
    # } else if(y_is_flowtibble && ! x_is_flowtibble){
    #     varname <- x$var[1]
    #     x$var = NULL
    # } else if(! x_is_flowtibble && ! y_is_flowtibble){
    #     varname <- x$var[1]
    #     x$var = NULL
    #     y$var = NULL
    # } else {
    #     stop('x and y are both "flow" tibbles. There should be no need for this')
    # }
    # if(x_is_flowtibble) x <- rename(x, val = flow)
    # if(y_is_flowtibble) y <- rename(y, val = flow)
    
    #data.table doesn't work with the errors package, so error needs
    #to be separated into its own column. also give same-name columns suffixes
    
    if('val' %in% colnames(x)){ #crude catch for nonstandard ms tibbles (fine for now)
        x <- x %>%
            mutate(err = errors::errors(val),
                   val = errors::drop_errors(val)) %>%
            rename_with(.fn = ~paste0(., '_x'),
                        .cols = everything()) %>%
            # .cols = any_of(c('site_code', 'var', 'val',
            #                  'ms_status', 'ms_interp'))) %>%
            data.table::as.data.table()
        
        y <- y %>%
            mutate(err = errors::errors(val),
                   val = errors::drop_errors(val)) %>%
            rename_with(.fn = ~paste0(., '_y'),
                        .cols = everything()) %>%
            data.table::as.data.table()
    } else {
        x <- dplyr::rename(x, datetime_x = datetime) %>% data.table::as.data.table()
        y <- dplyr::rename(y, datetime_y = datetime) %>% data.table::as.data.table()
    }
    
    #alternative implementation of the "on" argument in data.table joins...
    #probably more flexible, so leaving it here in case we need to do something crazy
    # data.table::setkeyv(x, 'datetime')
    # data.table::setkeyv(y, 'datetime')
    
    #convert the desired maximum roll distance from string to integer seconds
    rollmax <- ifelse(test = rollmax == '7:30',
                      yes = 7 * 60 + 30,
                      no = 12 * 60 * 60)
    
    #leaving this here in case the nearest neighbor join implemented below is too
    #slow. then we can fall back to a basic rolling join with a maximum distance
    # rollmax <- ifelse(test = direction == 'forward',
    #                   yes = -rollmax,
    #                   no = rollmax)
    #rollends will move the first/last value of x in the opposite `direction` if necessary
    # joined <- y[x, on = 'datetime', roll = rollmax, rollends = c(TRUE, TRUE)]
    
    #create columns in x that represent the snapping window around each datetime
    x[, `:=` (datetime_min = datetime_x - rollmax,
              datetime_max = datetime_x + rollmax)]
    y[, `:=` (datetime_y_orig = datetime_y)] #datetime col will be dropped from y
    
    # if(indices_only){
    #     y_indices <- y[x,
    #                    on = .(datetime_y <= datetime_max,
    #                           datetime_y >= datetime_min),
    #                    which = TRUE]
    #     return(y_indices)
    # }
    
    #join x rows to y if y's datetime falls within the x range
    joined <- y[x, on = .(datetime_y <= datetime_max,
                          datetime_y >= datetime_min)]
    joined <- na.omit(joined, cols = 'datetime_y_orig') #drop rows without matches
    
    #for any datetimes in x or y that were matched more than once, keep only
    #the nearest match
    joined[, `:=` (datetime_match_diff = abs(datetime_x - datetime_y_orig))]
    joined <- joined[, .SD[which.min(datetime_match_diff)], by = datetime_x]
    joined <- joined[, .SD[which.min(datetime_match_diff)], by = datetime_y_orig]
    
    if(indices_only){
        y_indices <- which(y$datetime_y %in% joined$datetime_y_orig)
        x_indices <- which(x$datetime_x %in% joined$datetime_x)
        return(list(x = x_indices, y = y_indices))
    }
    
    #drop and rename columns (data.table makes weird name modifications)
    if(keep_datetimes_from == 'x'){
        joined[, c('datetime_y', 'datetime_y.1', 'datetime_y_orig', 'datetime_match_diff') := NULL]
        data.table::setnames(joined, 'datetime_x', 'datetime')
    } else {
        joined[, c('datetime_x', 'datetime_y.1', 'datetime_y', 'datetime_match_diff') := NULL]
        data.table::setnames(joined, 'datetime_y_orig', 'datetime')
    }
    
    #restore error objects, var column, original column names (with suffixes).
    #original column order
    joined <- as_tibble(joined) %>%
        mutate(val_x = errors::set_errors(val_x, err_x),
               val_y = errors::set_errors(val_y, err_y)) %>%
        select(-err_x, -err_y)
    # mutate(var = !!varname)
    
    # if(x_is_flowtibble) joined <- rename(joined,
    #                                      flow = val_x,
    #                                      ms_status_flow = ms_status_x,
    #                                      ms_interp_flow = ms_interp_x)
    # if(y_is_flowtibble) joined <- rename(joined,
    #                                      flow = val_y,
    #                                      ms_status_flow = ms_status_y,
    #                                      ms_interp_flow = ms_interp_y)
    
    # if(! sum(grepl('^val_[xy]$', colnames(joined))) > 1){
    #     joined <- rename(joined, val = matches('^val_[xy]$'))
    # }
    
    joined <- select(joined,
                     datetime,
                     # matches('^val_?[xy]?$'),
                     # any_of('flow'),
                     starts_with('site_code'),
                     any_of(c(starts_with('var_'), matches('^var$'))),
                     any_of(c(starts_with('val_'), matches('^val$'))),
                     starts_with('ms_status_'),
                     starts_with('ms_interp_'))
    
    return(joined)
}

# ms_read_csv internals
gsub_v <- function(pattern, replacement_vec, x){
    
    #just like the first three arguments to gsub, except that
    #   replacement is now a vector of replacements.
    #return a vector of the same length as replacement_vec, where
    #   each element in replacement_vec has been used once
    
    subbed <- sapply(replacement_vec,
                     function(v) gsub(pattern = pattern,
                                      replacement = v,
                                      x = x),
                     USE.NAMES = FALSE)
    
    return(subbed)
}

resolve_datetime <- function(d,
                             datetime_colnames,
                             datetime_formats,
                             datetime_tz,
                             optional){
    
    #d: a data.frame or tibble with at least one date or time column
    #   (all date and/or time columns must contain character strings,
    #   not parsed date/time/datetime objects).
    #datetime_colnames: character vector; column names that contain
    #   relevant datetime information.
    #datetime_formats: character vector; datetime parsing tokens
    #   (like '%A, %Y-%m-%d %I:%M:%S %p' or '%j') corresponding to the
    #   elements of datetime_colnames.
    #datetime_tz: character; time zone of the returned datetime column.
    #optional: character vector; see dt_format_to_regex.
    
    #return value: d, but with a "datetime" column containing POSIXct datetimes
    #   and without the input datetime columns
    
    dt_tb <- tibble(basecol = rep(NA, nrow(d)))
    for(i in 1:length(datetime_colnames)){
        dt_comps <- str_match_all(string = datetime_formats[i],
                                  pattern = '%([a-zA-Z])')[[1]][,2]
        dt_regex <- dt_format_to_regex(datetime_formats[i],
                                       optional = optional)
        
        # for loop handling with number-of-character issues
        for(match in grepl("m|e|d|H|I|M|S", dt_comps)) {
            if(match){
                for (dt_entry in 1:nrow(d[datetime_colnames[i]])) {
                    if(! is.na(d[datetime_colnames[i]][dt_entry,])){
                        if(numbers_only(d[datetime_colnames[i]][dt_entry,])){
                            if(nchar(d[datetime_colnames[i]][dt_entry,]) == 1) {
                                d[datetime_colnames[i]][dt_entry,] <- paste0(0, d[datetime_colnames[i]][dt_entry,])
                            } else if(nchar(d[datetime_colnames[i]][dt_entry,]) ==3){
                                d[datetime_colnames[i]][dt_entry,] <- paste0(0, d[datetime_colnames[i]][dt_entry,])
                            }
                        }
                    }
                }
            }
        }
        
        dt_tb <- d %>%
            select(one_of(datetime_colnames[i])) %>%
            tidyr::extract(col = !!datetime_colnames[i],
                           into = dt_comps,
                           regex = dt_regex,
                           remove = TRUE,
                           convert = FALSE) %>%
            bind_cols(dt_tb)
    }
    
    dt_tb$basecol = NULL
    
    #fill in defaults if applicable:
    #(12 for hour, 00 for minute and second, PM for AM/PM)
    dt_tb <- dt_tb %>%
        mutate(
            # across(any_of(c('H', 'I')), ~ifelse(nchar(.x) < 2, paste0(0, .x), .x)),
            across(any_of(c('H', 'I')), ~ifelse(is.na(.x), '12', .x)),
            # across(any_of(c('M', 'S')), ~ifelse(nchar(.x) < 2, paste0(0, .x), .x)),
            across(any_of(c('M', 'S')), ~ifelse(is.na(.x), '00', .x)),
            across(any_of('p'), ~ifelse(is.na(.x), 'PM', .x)))
    
    #resolve datetime structure into POSIXct
    datetime_formats_split <- stringr::str_extract_all(datetime_formats,
                                                       '%[a-zA-Z]') %>%
        unlist()
    
    dt_col_order <- match(paste0('%',
                                 colnames(dt_tb)),
                          datetime_formats_split)
    
    if('H' %in% colnames(dt_tb)){
        dt_tb$H[dt_tb$H == ''] <- '00'
    }
    if('M' %in% colnames(dt_tb)){
        dt_tb$M[dt_tb$M == ''] <- '00'
    }
    if('S' %in% colnames(dt_tb)){
        dt_tb$S[dt_tb$S == ''] <- '00'
    }
    if('I' %in% colnames(dt_tb)){
        dt_tb$I[dt_tb$I == ''] <- '00'
    }
    if('P' %in% colnames(dt_tb)){
        dt_tb$P[dt_tb$P == ''] <- 'AM'
    }
    dt_tb <- dt_tb %>%
        tidyr::unite(col = 'datetime',
                     everything(),
                     sep = ' ',
                     remove = TRUE) %>%
        mutate(datetime = as_datetime(datetime,
                                      format = paste(datetime_formats_split[dt_col_order],
                                                     collapse = ' '),
                                      tz = datetime_tz) %>%
                   with_tz(tz = 'UTC'))
    d <- d %>%
        bind_cols(dt_tb) %>%
        select(-one_of(datetime_colnames), datetime) %>% #in case 'datetime' is in datetime_colnames
        relocate(datetime)
    
    return(d)
}


identify_sampling <- function(df,
                              is_sensor,
                              date_col = 'datetime',
                              network,
                              domain,
                              prodname_ms,
                              sampling_type){
    
    #TODO: for hbef, identify_sampling is writing sites names as 1 not w1
    
    #is_sensor: named logical vector. see documention for
    #   ms_read_raw_csv, but note that an unnamed logical vector of length one
    #   cannot be used here. also note that the original variable/flag column names
    #   from the raw file are converted to canonical macrosheds names by
    #   ms_read_raw_csv before it passes is_sensor to identify_sampling.
    
    #checks
    if(any(! is.logical(is_sensor))){
        stop('all values in is_sensor must be logical.')
    }
    
    svh_names <- names(is_sensor)
    if(is.null(svh_names) || any(is.na(svh_names))){
        stop('all elements of is_sensor must be named.')
    }
    
    #parse is_sensor into a character vector of sample regimen codes
    is_sensor <- ifelse(is_sensor, 'S', 'N')
    
    #set up directory system to store sample regimen metadata
    sampling_dir <- glue('data/{n}/{d}',
                         n = network,
                         d = domain)
    
    sampling_file <- glue('data/{n}/{d}/sampling_type.json',
                          n = network,
                          d = domain)
    
    master <- try(jsonlite::fromJSON(readr::read_file(sampling_file)),
                  silent = TRUE)
    
    if('try-error' %in% class(master)){
        dir.create(sampling_dir, recursive = TRUE)
        file.create(sampling_file)
        master <- list()
    }
    
    #determine and record sample regimen for each variable
    col_names <- colnames(df)
    
    data_cols <- grep(pattern = '__[|]dat',
                      col_names,
                      value = TRUE)
    
    flg_cols <- grep(pattern = '__[|]flg',
                     col_names,
                     value = TRUE)
    
    site_codes <- unique(df$site_code)
    
    for(p in 1:length(data_cols)){
        
        # var_name <- str_split_fixed(data_cols[p], '__', 2)[1]
        
        # df_var <- df %>%
        #     select(datetime, !!var_name := .data[[data_cols[p]]], site_code)
        
        all_sites <- tibble()
        for(i in 1:length(site_codes)){
            
            # df_site <- df_var %>%
            df_site <- df %>%
                filter(site_code == !!site_codes[i]) %>%
                arrange(datetime)
            # ! is.na(.data[[date_col]]), #NAs here are indicative of bugs we want to fix, so let's let them through
            # ! is.na(.data[[var_name]])) #NAs here are indicative of bugs we want to fix, so let's let them through
            
            dates <- df_site[[date_col]]
            dif <- diff(dates)
            unit <- attr(dif, 'units')
            
            conver_mins <- case_when(
                unit %in% c('seconds', 'secs') ~ 0.01666667,
                unit %in% c('minutes', 'mins') ~ 1,
                unit == 'hours' ~ 60,
                unit == 'days' ~ 1440,
                TRUE ~ NA_real_)
            
            if(is.na(conver_mins)) stop('Weird time unit encountered. address this.')
            
            dif_mins <- as.numeric(dif) * conver_mins
            dif_mins <- round(dif_mins)
            
            mode_mins <- Mode(dif_mins)
            mean_mins <- mean(dif_mins, na.rm = T)
            prop_mode_min <- length(dif_mins[dif_mins == mode_mins])/length(dif_mins)
            
            # remove gaps larger than 90 days (for seasonal sampling)
            dif_mins <- dif_mins[dif_mins < 129600]
            
            if(length(dif_mins) == 0){
                # This is grab
                g_a <- tibble('site_code' = site_codes[i],
                              'type' = 'G',
                              'starts' = min(dates, na.rm = TRUE),
                              'interval' = mode_mins)
            } else{
                if(prop_mode_min >= 0.3 && mode_mins <= 1440){
                    # This is installed
                    g_a <- tibble('site_code' = site_codes[i],
                                  'type' = 'I',
                                  'starts' = min(dates, na.rm = TRUE),
                                  'interval' = mode_mins)
                } else{
                    if(mean_mins <= 1440){
                        # This is installed (non standard interval like HBEF)
                        g_a <- tibble('site_code' = site_codes[i],
                                      'type' = 'I',
                                      'starts' = min(dates, na.rm = TRUE),
                                      'interval' = mean_mins)
                    } else{
                        # This is grab
                        g_a <- tibble('site_code' = site_codes[i],
                                      'type' = 'G',
                                      'starts' = min(dates, na.rm = TRUE),
                                      'interval' = mean_mins)
                    }
                }
            }
            
            if(! is.null(sampling_type)){
                g_a <- g_a %>%
                    mutate(type = sampling_type)
            }
            
            var_name_base <- str_split(string = data_cols[p],
                                       pattern = '__\\|')[[1]][1]
            
            g_a <- g_a %>%
                mutate(
                    type = paste0(type,
                                  !!is_sensor[var_name_base]),
                    var = as.character(glue('{ty}_{vb}',
                                            ty = type,
                                            vb = var_name_base)))
            
            master[[prodname_ms]][[var_name_base]][[site_codes[i]]] <-
                list('startdt' = g_a$starts,
                     'type' = g_a$type,
                     'interval' = g_a$interval)
            
            g_a <- g_a %>%
                mutate(interval = as.character(interval))
            
            all_sites <- bind_rows(all_sites, g_a)
        }
        
        #include new prefixes in df column names
        prefixed_varname <- all_sites$var[1]
        
        dat_colname <- paste0(drop_var_prefix(prefixed_varname),
                              '__|dat')
        flg_colname <- paste0(drop_var_prefix(prefixed_varname),
                              '__|flg')
        
        data_col_ind <- match(dat_colname,
                              colnames(df))
        flag_col_ind <- match(flg_colname,
                              colnames(df))
        
        colnames(df)[data_col_ind] <- paste0(prefixed_varname,
                                             '__|dat')
        colnames(df)[flag_col_ind] <- paste0(prefixed_varname,
                                             '__|flg')
    }
    
    readr::write_file(jsonlite::toJSON(master), sampling_file)
    
    return(df)
}

Mode <- function(x, na.rm = TRUE){
    
    if(na.rm){
        x <- na.omit(x)
    }
    
    ux <- unique(x)
    mode_out <- ux[which.max(tabulate(match(x, ux)))]
    return(mode_out)
    
}

numeric_any_v <- function(...){ #attack of the ellipses
    
    #...: numeric vectors of equal length. should be just 0s and 1s, but
    #   integers other than 1 are also considered TRUE by as.logical()
    
    #the vectorized version of numeric_any. good for stuff like:
    #    mutate(ms_status = numeric_any(c(ms_status_x, ms_status_flow)))
    
    #returns a single vector of the same length as arguments
    
    #this func could be useful in global situations
    numeric_any_positional <- function(...) numeric_any(c(...))
    
    numeric_any_elementwise <- function(...){
        Map(function(...) numeric_any_positional(...), ...)
    }
    
    out <- do.call(numeric_any_elementwise,
                   args = list(...)) %>%
        unlist()
    
    if(is.null(out)) out <- numeric()
    
    return(out)
}

get_response_1char <- function(msg,
                               possible_chars,
                               subsequent_prompt = FALSE){

    #msg: character. a message that will be used to prompt the user
    #possible_chars: character vector of acceptable single-character responses
    #subsequent prompt: not to be set directly. This is handled by
    #   get_response_mchar during recursion.

    if(subsequent_prompt){
        cat(paste('Please choose one of:',
                  paste(possible_chars,
                        collapse = ', '),
                  '\n> '))
    } else {
        cat(msg)
    }

    ch <- as.character(readLines(con = stdin(), 1))

    if(length(ch) == 1 && ch %in% possible_chars){
        return(ch)
    } else {
        get_response_1char(msg = msg,
                           possible_chars = possible_chars,
                           subsequent_prompt = TRUE)
    }
}
