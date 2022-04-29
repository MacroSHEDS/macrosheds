#' Watershed Boundary for the Catch la Poudre river
#'
#' Data was retrieved using the nhdplusTools package
#'
#' @format A \code{sf} object containing two columns 
#' \describe{
#'   \item{site_code}{name of site}
#'   \item{geometry}{geospatial information for the watershed}
#' }
#' 
#' @references Blodgett, D., 2019, nhdplusTools: Tools for Accessing and Working with the NHDPlus, https://code.usgs.gov/water/nhdplusTools
#'
#' @example 
#' This watershed was retrieved as follows:
#' start_point <- sf::st_as_sf(data.frame(x = -105.077466, y = 40.595203), 
#' coords = c("x", "y"), crs = 4326)
#' start_comid <- nhdplusTools::discover_nhdplus_id(start_point)
#' 
#' flowline <- nhdplusTools::navigate_nldi(list(featureSource = "comid",
#'                                              featureID = start_comid),
#'                                         mode = "upstreamTributaries",
#'                                         distance_km = 1000)
#' 
#' subset_file <- tempfile(fileext = ".gpkg")
#' subset <- nhdplusTools::subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
#'                                        output_file = subset_file,
#'                                        nhdplus_data = "download",
#'                                        flowline_only = FALSE,
#'                                        return_data = TRUE, overwrite = TRUE)
#' 
#' watersheds <- subset$CatchmentSP %>%
#'     sf::st_union() %>%
#'     st_as_sf()
#' 
#' poudre_ws <- watersheds %>%
#'     mutate(site_code = 'poudre_river') %>%
#'     rename(geometry = x)
#' 
#' To use this data:
#' data(poudre_ws)
#' print(poudre_ws)
#' mapview::mapview(poudre_ws)
"poudre_ws"