#' Download the MacroSheds site-variable catalog
#'
#' downloads the MacroSheds catalog, which documents
#' important metadata about what variables are recorded at what sites,
#' and the number of observations, first and last record, and other
#' important metadata for all site-variable pairs.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param  fp character. provide a valid filepath to download the catalog to.
#' @return returns a \code{data.frame} of the variables in
#'         the MacroSheds dataset, the sites that record them,
#'         number of observations, and more.
#' @export
#' @seealso [ms_download_variables()], [ms_download_site_data()]
#' @examples
#' site_vars_catalog <- ms_catalog()

ms_catalog <- function(fp = NULL){

    ms_cat <- readr::read_csv('https://figshare.com/articles/dataset/variable_catalog/19585810/files/38857869',
                        col_types = readr::cols())
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
