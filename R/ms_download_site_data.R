#' Download macrosheds site dataset
#'
#' Download the macrosheds site datasets of stream chemistry, stream
#' discharge, stream chemical flux, precipitation, precipitation chemistry,
#' precipitation chemical flux, ws_boundary, stream_gauge_locations, and
#' precip_gauge_locations. Not all products are available at all sites, but
#' all products available for the selected domains will be downloaded.
#'
#' @author Spencer Rhea, \email{spencerrhea41@@gmail.com}
#' @author Mike Vlah
#' @author Wes Slaughter
#' @param macrosheds_root character. Directory where macrosheds data files will be downloaded.
#'    If this directory does not exist, it will be created.
#' @param networks character vector. macrosheds networks that will be downloaded.
#'    Either a single network, vector of networks, or 'all'. See \code{ms_downloadsite_data()}
#'    for networks available for download.
#' @param domains character vector. macrosheds domains that will be downloaded.
#'    Either a single domain, vector of domains, or 'all'. See \code{ms_downloadsite_data()}
#'    for domains available for download.
#' @param quiet logical. If TRUE, some messages will be suppressed.
#' @return Downloads all site data for selected domains to the
#'    directory specified by \code{macrosheds_root}. Site datasets are arranged according to the following
#'    structure: domain/prodname/site_code.feather. For definitions of these terms as used by
#'    MacroSheds, see [MacroSheds documentation](https://doi.org/10.6084/m9.figshare.c.5621740).
#'
#' @details Either \code{networks} or \code{domains} must be supplied. If 'all' is
#'    supplied to either argument, all domains will be downloaded regardless of
#'    what is supplied to the other argument. The full (site) dataset is approximately 500 MiB when
#'    compressed and may take minutes to hours to download, depending on connection speed. When uncompressed,
#'    the dataset is approximately 4 GiB, so be mindful of disk space.
#' @export
#' @seealso [ms_load_sites()], [ms_load_variables()], [ms_load_product()], [ms_load_spatial_product()]
#' @examples
#' dir.create('data/macrosheds', recursive = TRUE)
#' ms_download_site_data(macrosheds_root = 'data/macrosheds',
#'                       domains = c('niwot', 'hjandrews'))

ms_download_site_data <- function() {
  warning('ms_download_site_data() has been deprecated, use ms_load_sites() instead')
}
