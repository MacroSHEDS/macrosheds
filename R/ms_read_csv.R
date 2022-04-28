#' read a csv into macrosheds format
#'
#' read in csv data that meets minimum criteria, built to be as robust
#' as possible to heterogeneous data formats and contents. 
#'
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' 
#' @param filepath character. path to local CSV.
#' @param preprocessed_tibble tibble. a tibble with all character columns. Supply this argument
#'    if a dataset requires modification before it can be processed by ms_read_raw_csv.
#'    This may be necessary if, e.g. time is stored in a format that can't be parsed by
#'    standard datetime format strings. Either filepath or preprocessed_tibble must be
#'    supplied, but not both.
#' @param datetime_cols named character vector. names are column names that
#     contain components of a datetime. values are format strings (e.g.
#     '%Y-%m-%d', '%H') corresponding to the datetime components in those
#     columns.
#' @param datetime_tz character. specifying time zone. this specification must be
#'    among those provided by OlsonNames()
#' @param optionalize_nontoken_characters character vector. used when there might be
#     variation in date/time formatting within a column. in regex speak,
#     optionalizing a token string means, "match this string if it exists,
#     but move on to the next token if it doesn't." All datetime parsing tokens
#     (like "%H") are optionalized automatically when this function converts
#     them to regex. But other tokens like ":" and "-" that might be used in
#     datetime strings are not. Concretely, if you wanted to read either "%H:%M:%S"
#     or "%H:%M" in the same column, you'd set optionalize_nontoken_characters = ':',
#     and then the parser wouldn't require there to be two colons in order to
#     match the string. Don't use this if you don't have to, because it reduces
#     specificity. See "optional" argument to dt_format_to_regex for more details.
#' @param site_code_col character. name of column containing site name information
#' @param alt_site_code optional list. Names of list elements are desired site_codes
#'    within MacroSheds. List elements are character vectors of alternative
#'    names that might be encountered. Used when sites are misnamed or need
#'    to be changed due to inconsistencies within and across datasets.
#' @param data_cols vector. vector of names of columns containing data. If elements of this
#'   vector are named, names are taken to be the column names as they exist
#'   in the file, and values are used to replace those names. Data columns that
#'   aren't referred to in this argument will be omitted from the output,
#'   as will their associated flag columns (if any).
#' @param data_col_pattern character. a string containing the wildcard "#V#",
#'   which represents any number of characters. If data column names will be
#'   used as-is, this wildcard is all you need. if data columns contain
#'   recurring, superfluous characters, you can omit them with regex. for
#'   example, if data columns are named outflow_x, outflow_y, outflow_...., use
#'   data_col_pattern = 'outflow_#V#' and then you don't have to bother
#'   typing the full names in your argument to data_cols.
#' @param alt_datacol_pattern optional string with same mechanics as data_col_pattern.
#'   use this if there might be a second way in which column names are generated, e.g.
#'   output_x, output_y, output_....
#' @param is_sensor logical. either a single logical value, which will be applied to all
#'   variable columns OR a named logical vector with the same length and names as
#'   data_cols. If the latter, names correspond to variable names in the file to be read.
#'   TRUE means the corresponding variable(s) was/were
#'   measured with a sensor (which may be susceptible to drift and/or fouling),
#'   FALSE means the measurement(s) was/were not recorded by a sensor. This
#'   category includes analytical measurement in a lab, visual recording, etc.
#' @param set_to_NA character. For values such as 9999 that are proxies for NA values.
#' @param var_flagcol_pattern character. optional string with same mechanics as the other
#'   pattern parameters. this one is for columns containing flag
#'   information that is specific to one variable. If there's only one
#'   data column, omit this argument and use summary_flagcols for all
#'   flag information.
#' @param alt_varflagcol_pattern character. optional string with same mechanics as the other
#'   pattern parameters. just in case there are two naming conventions for variable-specific
#'   flag columns
#' @param summary_flagcols vector. optional unnamed vector of column names for flag columns
#'   that pertain to all variables
#' @param sampling_type optional value to overwrite identify_sampling because
#'      some . vector's function is misidentifying sampling type. This must b
#'      single . vector 'G or I and is applied to all variables in product
#'
#' @return returns a tibble of ordered and renamed columns, omitting any columns
#'   from the original file that do not contain data, flag/qaqc information,
#'   datetime, or site_code. All-NA data columns and their corresponding
#'   flag columns will also be omitted, as will rows where all data values
#'   are NA. Rows with NA in the datetime or site_code column are dropped.
#'   data columns are given type double. all other
#'   columns are given type character. data and flag/qaqc columns are
#'   given two-letter prefixes representing sample regimen
#'   (I = installed vs. G = grab; S = sensor vs N = non-sensor).
#'   Data and flag/qaqc columns are also given
#'   suffixes (__|flg and __|dat) that allow them to be cast into long format
#'   by ms_cast_and_reflag.

ms_read_csv <- function(filepath,
                            preprocessed_tibble,
                            datetime_cols,
                            datetime_tz,
                            optionalize_nontoken_characters = ':',
                            site_code_col,
                            alt_site_code,
                            data_cols,
                            data_col_pattern,
                            alt_datacol_pattern,
                            is_sensor,
                            set_to_NA,
                            var_flagcol_pattern,
                            alt_varflagcol_pattern,
                            summary_flagcols,
                            sampling_type = NULL){
    
    #TODO:
    #add a silent = TRUE option. this would hide all warnings
    #allow a vector of possible matches for each element of datetime_cols
    #   (i.e. make it take a list of named vectors)
    #write more checks for improper specification.
    #if file to be read is stored in long format, this function will not work!
    #this could easily be adapted to read other delimited filetypes.
    #could also add a drop_empty_rows and/or drop_empty_datacols parameter.
    #   atm those things happen automatically
    #likewise, a remove_duplicates param could be nice. atm, for duplicated rows,
    #   the one with the fewest NA values is kept automatically
    #allow a third option in is_sensor for mixed sensor/nonsensor
    #   (also change param name). check for comments inside munge kernels
    #   indicating where this is needed
    #site_code_col should eventually work like datetime_cols (in case site_code is
    #   separated into multiple components)
    



    #checks
    filepath_supplied <-  ! missing(filepath) && ! is.null(filepath)
    tibble_supplied <-  ! missing(preprocessed_tibble) && ! is.null(preprocessed_tibble)
    
    if(filepath_supplied && tibble_supplied){
        stop(glue('Only one of filepath and preprocessed_tibble can be supplied. ',
                  'preprocessed_tibble is for rare circumstances only.'))
    }
    
    
    if(! datetime_tz %in% OlsonNames()){
        stop('datetime_tz must be included in OlsonNames()')
    }
    
    if(length(data_cols) == 1 &&
       ! (missing(var_flagcol_pattern) || is.null(var_flagcol_pattern))){
        stop(paste0('Only one data column. Use summary_flagcols instead ',
                    'of var_flagcol_pattern.'))
    }
    
    if(any(! is.logical(is_sensor))){
        stop('all values in is_sensor must be logical.')
    }
    
    svh_names <- names(is_sensor)
    if(
        length(is_sensor) != 1 &&
        (is.null(svh_names) || any(is.na(svh_names)))
    ){
        stop('if is_sensor is not length 1, all elements must be named.')
    }
    
    if(! all(data_cols %in% ms_vars$variable_code)) {
        
        for(i in 1:length(data_cols)) {
            if(!data_cols[i] %in% ms_vars$variable_code) {
                logerror(msg = paste(unname(data_cols[i]), 'is not in varibles.csv; add'),
                         logger = logger_module)
            }
        }
    }
    
    if(! is.null(sampling_type)){
        if(! length(sampling_type) == 1){
            stop('sampling_type must be a length of 1')
        }
        if(! sampling_type %in% c('G', 'I')){
            stop('sampling_type must be either I or G')
        }
    }
    
    #parse args; deal with missing args
    datetime_colnames <- names(datetime_cols)
    datetime_formats <- unname(datetime_cols)
    
    alt_datacols <- var_flagcols <- alt_varflagcols <- NA
    alt_datacol_names <- var_flagcol_names <- alt_varflagcol_names <- NA
    if(missing(summary_flagcols)){
        summary_flagcols <- NULL
    }
    
    if(missing(set_to_NA)) {
        set_to_NA <- NULL
    }
    
    if(missing(alt_site_code)) {
        alt_site_code <- NULL
    }
    
    #fill in missing names in data_cols (for columns that are already
    #   canonically named)
    datacol_names0 <- names(data_cols)
    if(is.null(datacol_names0)) datacol_names0 <- rep('', length(data_cols))
    datacol_names0[datacol_names0 == ''] <-
        unname(data_cols[datacol_names0 == ''])
    
    #expand data columnname wildcards and rename data_cols
    datacol_names <- gsub_v(pattern = '#V#',
                            replacement_vec = datacol_names0,
                            x = data_col_pattern)
    names(data_cols) <- datacol_names
    
    #expand alternative data columnname wildcards and populate alt_datacols
    if(! missing(alt_datacol_pattern) && ! is.null(alt_datacol_pattern)){
        alt_datacols <- data_cols
        alt_datacol_names <- gsub_v(pattern = '#V#',
                                    replacement_vec = datacol_names0,
                                    x = alt_datacol_pattern)
        names(alt_datacols) <- alt_datacol_names
    }
    
    #expand varflag columnname wildcards and populate var_flagcols
    if(! missing(var_flagcol_pattern) && ! is.null(var_flagcol_pattern)){
        var_flagcols <- data_cols
        var_flagcol_names <- gsub_v(pattern = '#V#',
                                    replacement_vec = datacol_names0,
                                    x = var_flagcol_pattern)
        names(var_flagcols) <- var_flagcol_names
    }
    
    #expand alt varflag columnname wildcards and populate alt_varflagcols
    if(! missing(alt_varflagcol_pattern) && ! is.null(alt_varflagcol_pattern)){
        alt_varflagcols <- data_cols
        alt_varflagcol_names <- gsub_v(pattern = '#V#',
                                       replacement_vec = datacol_names0,
                                       x = alt_varflagcol_pattern)
        names(alt_varflagcols) <- alt_varflagcol_names
    }
    
    #combine all available column name mappings; assemble new name vector
    colnames_all <- c(data_cols, alt_datacols, var_flagcols, alt_varflagcols)
    na_inds <- is.na(colnames_all)
    colnames_all <- colnames_all[! na_inds]
    
    suffixes <- rep(c('__|dat', '__|dat', '__|flg', '__|flg'),
                    times = c(length(data_cols),
                              length(alt_datacols),
                              length(var_flagcols),
                              length(alt_varflagcols)))
    
    suffixes <- suffixes[! na_inds]
    colnames_new <- paste0(colnames_all, suffixes)
    
    colnames_all <- c(datetime_colnames, colnames_all)
    names(colnames_all)[1:length(datetime_cols)] <- datetime_colnames
    colnames_new <- c(datetime_colnames, colnames_new)
    
    if(! missing(site_code_col) && ! is.null(site_code_col)){
        colnames_all <- c('site_code', colnames_all)
        names(colnames_all)[1] <- site_code_col
        colnames_new <- c('site_code', colnames_new)
    }
    
    if(! is.null(summary_flagcols)){
        nsumcol <- length(summary_flagcols)
        summary_flagcols_named <- summary_flagcols
        names(summary_flagcols_named) <- summary_flagcols
        colnames_all <- c(colnames_all, summary_flagcols_named)
        colnames_new <- c(colnames_new, summary_flagcols)
    }
    
    #assemble colClasses argument to read.csv
    classes_d1 <- rep('numeric', length(data_cols))
    names(classes_d1) <- datacol_names
    
    classes_d2 <- rep('numeric', length(alt_datacols))
    names(classes_d2) <- alt_datacol_names
    
    classes_f1 <- rep('character', length(var_flagcols))
    names(classes_f1) <- var_flagcol_names
    
    classes_f2 <- rep('character', length(alt_varflagcols))
    names(classes_f2) <- alt_varflagcol_names
    
    classes_f3 <- rep('character', length(summary_flagcols))
    names(classes_f3) <- summary_flagcols
    
    class_dt <- rep('character', length(datetime_cols))
    names(class_dt) <- datetime_colnames
    
    if(! missing(site_code_col) && ! is.null(site_code_col)){
        class_sn <- 'character'
        names(class_sn) <- site_code_col
    }
    
    classes_all <- c(class_dt, class_sn, classes_d1, classes_d2, classes_f1,
                     classes_f2, classes_f3)
    classes_all <- classes_all[! is.na(names(classes_all))]
    
    if(filepath_supplied){
        d <- read.csv(filepath,
                      stringsAsFactors = FALSE,
                      colClasses = "character")
    } else {
        d <- mutate(preprocessed_tibble,
                    across(everything(), as.character))
    }
    
    d <- d %>%
        as_tibble() %>%
        select(one_of(c(names(colnames_all), 'NA.'))) #for NA as in sodium
    if('NA.' %in% colnames(d)) class(d$NA.) = 'character'
    
    # Remove any variable flags created by pattern but do not exist in data
    # colnames_all <- colnames_all[names(colnames_all) %in% names(d)]
    # classes_all <- classes_all[names(classes_all) %in% names(d)]
    
    # Set values to NA if used as a flag or missing data indication
    # Not sure why %in% does not work, seem to only operate on one row
    if(! is.null(set_to_NA)){
        for(i in 1:length(set_to_NA)){
            d[d == set_to_NA[i]] <- NA
        }
    }
    
    #Set correct class for each column
    colnames_d <- colnames(d)
    
    for(i in 1:ncol(d)){
        
        if(colnames_d[i] == 'NA.'){
            class(d[[i]]) <- 'numeric'
            next
        }
        
        class(d[[i]]) <- classes_all[names(classes_all) == colnames_d[i]]
    }
    # d[] <- sw(Map(`class<-`, d, classes_all)) #sometimes classes_all is too long, which makes this fail
    
    #rename cols to canonical names
    for(i in 1:ncol(d)){
        
        if(colnames_d[i] == 'NA.'){
            colnames_d[i] <- 'Na__|dat'
            next
        }
        
        canonical_name_ind <- names(colnames_all) == colnames_d[i]
        if(any(canonical_name_ind)){
            colnames_d[i] <- colnames_new[canonical_name_ind]
        }
    }
    
    colnames(d) <- colnames_d
    
    #resolve datetime structure into POSIXct
    d  <- resolve_datetime(d = d,
                           datetime_colnames = datetime_colnames,
                           datetime_formats = datetime_formats,
                           datetime_tz = datetime_tz,
                           optional = optionalize_nontoken_characters)
    
    #remove rows with NA in datetime or site_code
    d <- filter(d,
                across(any_of(c('datetime', 'site_code')),
                       ~ ! is.na(.x)))
    #remove all-NA data columns and rows with NA in all data columns.
    #also remove flag columns for all-NA data columns.
    all_na_cols_bool <- apply(select(d, ends_with('__|dat')),
                              MARGIN = 2,
                              function(x) all(is.na(x)))
    all_na_cols <- names(all_na_cols_bool[all_na_cols_bool])
    all_na_cols <- c(all_na_cols,
                     sub(pattern = '__\\|dat',
                         replacement = '__|flg',
                         all_na_cols))
    
    d <- d %>%
        select(-one_of(all_na_cols)) %>%
        filter_at(vars(ends_with('__|dat')),
                  any_vars(! is.na(.)))
    
    #for duplicated datetime-site_code pairs, keep the row with the fewest NA
    #   values. We could instead do something more sophisticated.
    d <- d %>%
        rowwise(one_of(c('datetime', 'site_code'))) %>%
        mutate(NAsum = sum(is.na(c_across(ends_with('__|dat'))))) %>%
        ungroup() %>%
        arrange(datetime, site_code, NAsum) %>%
        select(-NAsum) %>%
        distinct(datetime, site_code, .keep_all = TRUE) %>%
        arrange(site_code, datetime)
    
    #convert NaNs to NAs, just in case.
    d[is.na(d)] <- NA
    
    #either assemble or reorder is_sensor to match names in data_cols
    if(length(is_sensor) == 1){
        
        is_sensor <- rep(is_sensor,
                         length(data_cols))
        names(is_sensor) <- unname(data_cols)
        
    } else {
        
        data_col_order <- match(names(is_sensor),
                                names(data_cols))
        is_sensor <- is_sensor[data_col_order]
    }
    
    #fix sites names if multiple names refer to the same site
    if(! is.null(alt_site_code)){
        
        for(z in 1:length(alt_site_code)){
            
            d <- mutate(d,
                        site_code = ifelse(site_code %in% !!alt_site_code[[z]],
                                           !!names(alt_site_code)[z],
                                           site_code))
        }
    }
    
    #prepend two-letter code to each variable representing sample regimen and
    #record sample regimen metadata
    d <- sm(identify_sampling(df = d,
                              is_sensor = is_sensor,
                              domain = domain,
                              network = network,
                              prodname_ms = prodname_ms,
                              sampling_type = sampling_type))
    
    #Check if all sites are in site file
    unq_sites <- unique(d$site_code)
    if(! all(unq_sites %in% site_data$site_code)){
        
        for(i in seq_along(unq_sites)) {
            if(! unq_sites[i] %in% site_data$site_code){
                logwarn(msg = paste(unname(unq_sites[i]),
                                    'is not in site_data file; add?'),
                        logger = logger_module)
            }
        }
    }
    
    return(d)
}
