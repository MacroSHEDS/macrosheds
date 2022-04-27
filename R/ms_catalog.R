#' Query MacroSheds variable catalog
#'
#' downloads the MacroSheds variable catalog, which documents
#' important metadata for all variables in MacroSheds -- such
#' as sites, number of observations, first and last record of
#' at each site, and more.
#'
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of the variables in
#'         the MacroSheds dataset and metadata about them.
#' @export
#' @seealso [ms_download_variables()], [ms_download_site_data()]
#' @examples
#' macrosheds_var_catalog <- ms_download_var_catalog()

ms_catalog <- function(fp = NULL){

    ms_cat <- read_csv('https://figshare.com/articles/dataset/variable_catalog/19585810/files/34791658',
                        col_types = cols())
    ms_cat <- ms_cat %>%
      select(-chem_category)

    # allow local download if file path supplied
    if(!is.null(fp)) {
      tryCatch(
        expr = {
          write.csv(ms_cat, fp)
        },
        error = function(e) {
          print(paste("file failed to write to:", fp))
        }
      )
    }

    warning('NEON data present in catalog but not yet available in public version of dataset')
    return(ms_cat)
}
