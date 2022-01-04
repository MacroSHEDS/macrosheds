#' Internal functions
#' 
#' Not intended to be called directly by the user
#' 
#' @keywords internal 

# Function aliases 
sw = suppressWarnings
sm = suppressWarnings
# end function aliases 

list_all_product_dirs <- function(macrosheds_root, prodname){
    
    prodname_dirs <- list.dirs(path = macrosheds_root,
                               full.names = TRUE,
                               recursive = TRUE)
    
    prodname_dirs <- grep(pattern = paste0(prodname, '__'),
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
    
    old_top_unit <- str_split_fixed(old_top, "", 2)[1]
    
    if(old_top_unit %in% c('g', 'e', 'q', 'l') || old_fraction[1] == 'mol') {
        old_top_conver <- 1
    } else {
        old_top_conver <- as.numeric(filter(units, prefix == old_top_unit)[,2])
    }
    
    old_bottom_unit <- str_split_fixed(old_bottom, "", 2)[1]
    
    if(old_bottom_unit %in% c('g', 'e', 'q', 'l') || old_fraction[2] == 'mol') {
        old_bottom_conver <- 1
    } else {
        old_bottom_conver <- as.numeric(filter(units, prefix == old_bottom_unit)[,2])
    }
    
    new_top_unit <- str_split_fixed(new_top, "", 2)[1]
    
    if(new_top_unit %in% c('g', 'e', 'q', 'l') || new_fraction[1] == 'mol') {
        new_top_conver <- 1
    } else {
        new_top_conver <- as.numeric(filter(units, prefix == new_top_unit)[,2])
    }
    
    new_bottom_unit <- str_split_fixed(new_bottom, "", 2)[1]
    
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
    
    year <- year(dates)
    
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
    
    DecYear <- (yday(dates)/days_in_year)+year(dates)
    
    return(DecYear)
}
get_MonthSeq <- function(dates){
    
    years <- year(dates)-1850
    
    MonthSeq <- years*12
    
    MonthSeq <- MonthSeq + month(dates)
    
    return(MonthSeq)
}
get_start_end <- function(d){
    start_date <- min(d$datetime)
    start_year <- year(start_date)
    start_wy <- ifelse(month(start_date) %in% c(10, 11, 12), start_year+1, start_year)
    filter_start_date <- lubridate::ymd(paste0(start_wy, '-10-01'))
    
    end_date <- max(d$datetime)
    end_year <- year(end_date)
    end_wy <- ifelse(month(end_date) %in% c(10, 11, 12), end_year+1, end_year)
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