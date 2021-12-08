#' Convert macrosheds concentration units
#'
#' Convert macrosheds data from it's native format into units of g/l, mol/l, and 
#' eq/l. Along with metric base unit conversions and conversions from molecules 
#' in the form of NO3 as N (NO3_N) to NO3 (NO3) and vice versa 
#'
#' @author Spencer Rhea, \email{spencerrhea41@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param d \code{date.frame}. A macrosheds \code{dataframe} of concentration data 
#'    downloaded using \code{download_ms_core_data()}.
#' @param convert_units_from a named character vector or character.
#'    Names are variable names without their sample-regimen prefixes 
#'    (e.g. 'DIC', not 'GN_DIC'), and values are the units of those variables. 
#'    Omit variables that don't need to be converted. Or a single unit if all 
#'    variables are in the same unit.
#' @param convert_units_to a named character vector or character. Names are 
#'    variable names without their sample-regimen prefixes (e.g. 'DIC', not 'GN_DIC'),
#'    and values are the units those variables should be converted to.
#'    Omit variables that don't need to be converted. Or a single unit if all 
#'    variables will be converted to the same unit.
#' @param convert_molecules character vector. A vector of variables that will be 
#'    converted from or to the atomic mass of a molecule's main constituent 
#'    (NO3_N to NO3 or NO3 to NO3_N), see details. 
#' @return returns a \code{tibble} containing concentration data converted to new units
#' @details convert_molecules is used to convert to and from a variable reported
#'    as the atomic weight of the main constituent in a molecule. Macrosheds
#'    native format is a molecule reported as the atomic weight of the main 
#'    constituent. Examples include NO3 as N (NO3_N), SO4 as S (SO4_S), SiO2 as Si
#'    (SiO2_as Si). If you wanted to convert from the native macrosheds format of
#'    NO3_N to NO3, you would list the variable as it currently exists in the 
#'    \code{data.frame}, so in this case NO3_N. The function will then convert
#'    NO3 as N (NO3_N) to just NO3, along with other conversion listed in the convert_units_to
#'    argument.
#' @export
#' @examples
#' ### Load in macrosheds data 
#' d <- load_product(macrosheds_root = 'data/macrosheds_v1', 
#'                   prodname = 'stream_chemistry',
#'                   sort_result = FALSE,
#'                   domains = 'hbef',
#'                   filter_vars = c('NO3_N', 'Na', 'Mg', 'SO4_S'))
#'
#' ### Convert all variable from the macrosheds native untis of mg/l to ug/l
#' converted_data <- ms_conversions(d = d,
#'                                  convert_units_from = 'mg/l',
#'                                  convert_units_to = 'ug/l')
#'                                  
#' ### Convert variables reported as their atomic mass to the mass of the whole molecule
#' converted_data <- ms_conversions(d = d,
#'                                  convert_units_from = 'mg/l',
#'                                  convert_units_to = 'mg/l',
#'                                  convert_molecules = c('NO3_N', 'SO4_S'))
#'
#' ### Convert from mg/l to mmol/l
#' converted_data <- ms_conversions(d = d,
#'                                  convert_units_from = 'mg/l',
#'                                  convert_units_to = 'mmol/l')
#' ### Convert from mg/l to meq/l
#' converted_data <- ms_conversions(d = d,
#'                                  convert_units_from = 'mg/l',
#'                                  convert_units_to = 'meq/l')
#' 
#' ### Convert variables to different units
#' converted_data <- ms_conversions(d = d,
#'                                  convert_units_from = c('NO3_N' = 'mg/l', 
#'                                                         'Na' = 'mg/l', 
#'                                                         'Mg' = 'mg/l', 
#'                                                         'SO4_S' = 'mg/l'),
#'                                  convert_units_to = c('NO3_N' = 'meq/l',
#'                                                       'Na' = 'ug/l',
#'                                                       'Mg' = 'umol/l',
#'                                                       'SO4_S' = 'g/l'))

ms_conversions <- function(d,
                           convert_units_from = 'mg/l',
                           convert_units_to,
                           convert_molecules){
    
    # d <- read_feather('data/ms_test/hbef/stream_chemistry__ms006/w6.feather') %>% filter(var == 'GN_NO3_N')
    # convert_molecules <- 'NO3'
    # convert_units_to = 'ug/l'
    # TEMPORARY (will replace when vars is on figshare)
    ms_vars <<- suppressMessages(read_csv('variables.csv'))
    
    
    #checks
    cm <- ! missing(convert_molecules)
    cuF <- ! missing(convert_units_from) && ! is.null(convert_units_from)
    cuT <- ! missing(convert_units_to) && ! is.null(convert_units_to)
    
    if(sum(cuF, cuT) == 1){
        stop('convert_units_from and convert_units_to must be supplied together')
    }
    if(length(convert_units_from) != length(convert_units_to)){
        stop('convert_units_from and convert_units_to must have the same length')
    }
    
    vars <- drop_var_prefix(d$var)
    
    vars_convertable <- ms_vars %>%
        filter(variable_code %in% !!vars) %>%
        pull(unit) %>%
        tolower()
    
    if(length(convert_units_from) == 1 && length(convert_units_to) == 1){
        if(!all(vars_convertable == convert_units_from)) {
            stop('all varibles in dataframe do not match convert_units_from, ensure all varibles are in the same unit or specify each varible in datafrae individually, see details for more information')
        }
    } else{
            cu_shared_names <- base::intersect(names(convert_units_from),
                                               names(convert_units_to))
            
            if(length(cu_shared_names) != length(convert_units_to)){
                stop('names of convert_units_from and convert_units_to must match')
            }
        }
    
    whole_molecule <- c('NO3', 'SO4', 'PO4', 'SiO2', 'SiO3', 'NH4', 'NH3',
                        'NO3_NO2')
    element_molecule <- c('NO3_N', 'SO4_S', 'PO4_P', 'SiO2_S', 'SiO3_S', 'NH4_N', 
                          'NH3_N', 'NO3_NO2_N')

    if(cm){
        whole_to_element <- grep(paste0(paste0('^', convert_molecules, '$'), collapse = '|'),
                                 whole_molecule)
        element_to_whole <- grep(paste0(paste0('^', convert_molecules, '$'), collapse = '|'), 
                                 element_molecule)
        
        if(length(element_to_whole) == 0 && length(whole_to_element) == 0){
            stop(paste0('convert_molecules must be one of: ', paste(whole_molecule, collapse = ' '),
                        ' or: ', paste(element_molecule, collapse = ' ')))
        }
    } else{
        convert_molecules <- NULL
    }
    # convert_molecules <- convert_molecules[! convert_molecules %in% keep_molecular]
    # convert_molecules <- convert_molecules[convert_molecules %in% unique(vars)]
    
    molecular_conversion_map <- list(
        NH4 = 'N',
        NO3 = 'N',
        NH3 = 'N',
        SiO2 = 'Si',
        SiO3 = 'Si',
        SO4 = 'S',
        PO4 = 'P',
        NO3_NO2 = 'N')
    
    #handle molecular conversions, like NO3 -> NO3_N
    if(cm && length(whole_to_element) > 0){
        convert_molecules_element <-  whole_molecule[whole_to_element]
        for(v in 1:length(convert_molecules_element)){
            
            d$val[vars == convert_molecules_element[v]] <- convert_molecule(x = d$val[vars == convert_molecules_element[v]],
                                                 from = convert_molecules_element[v],
                                                 to = unname(molecular_conversion_map[v]))
            
            check_double <- str_split_fixed(unname(molecular_conversion_map[v]), '', n = Inf)[1,]
            
            if(length(check_double) > 1 && length(unique(check_double)) == 1) {
                molecular_conversion_map[v] <- unique(check_double)
            }
            
            new_name <- paste0(d$var[vars == convert_molecules_element[v]], '_', unname(molecular_conversion_map[v]))
            
            d$var[vars == convert_molecules_element[v]] <- new_name
        }
    }
    
    #handle molecular conversions, like NO3_N -> NO3
    if(cm && length(element_to_whole) > 0){
        convert_molecules_element <-  element_molecule[element_to_whole]
        for(v in 1:length(convert_molecules_element)){
            
            d$val[vars == convert_molecules_element[v]] <- convert_molecule(x = d$val[vars == convert_molecules_element[v]],
                                                                            from = convert_molecules_element[v],
                                                                            to = whole_molecule[element_to_whole[v]])
            
            # check_double <- str_split_fixed(unname(molecular_conversion_map[v]), '', n = Inf)[1,]
            # 
            # if(length(check_double) > 1 && length(unique(check_double)) == 1) {
            #     molecular_conversion_map[v] <- unique(check_double)
            # }
            old_var <- unique(d$var[vars == convert_molecules_element[v]])
            new_name <- substr(d$var[vars == convert_molecules_element[v]], 0, nchar(old_var)-2)
            
            d$var[vars == convert_molecules_element[v]] <- new_name
        }
    }
    
    # Turn a single input into a named vector with all variables in dataframe 
    if(length(convert_units_from) == 1){
        all_vars <- unique(vars)
        convert_units_from <- rep(convert_units_from, length(all_vars))
        names(convert_units_from) <- all_vars
        convert_units_to <- rep(convert_units_to, length(all_vars))
        names(convert_units_to) <- all_vars
    }
    
    # Converts input to grams if the final unit contains grams
    for(i in 1:length(convert_units_from)){
        
        unitfrom <- convert_units_from[i]
        unitto <- convert_units_to[i]
        v <- names(unitfrom)
        
        g_conver <- FALSE
        if(grepl('mol|eq', unitfrom) && grepl('g', unitto) || v %in% convert_molecules){
            
            d$val[vars == v] <- convert_to_gl(x = d$val[vars == v],
                                              input_unit = unitfrom,
                                              molecule = v)
            
            g_conver <- TRUE
        }
        
        #convert prefix
        d$val[vars == v] <- convert_unit(x = d$val[vars == v],
                                         input_unit = unitfrom,
                                         output_unit = unitto)
        
        #Convert to mol or eq if that is the output unit
        if(grepl('mol|eq', unitto)) {
            
            d$val[vars == v] <- convert_from_gl(x = d$val[vars == v],
                                                input_unit = unitfrom,
                                                output_unit = unitto,
                                                molecule = v,
                                                g_conver = g_conver)
        }
    }
    
    return(d)
}
