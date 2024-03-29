% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_calc_watershed_precip.R
\name{ms_calc_watershed_precip}
\alias{ms_calc_watershed_precip}
\title{Spatially interpolate precipitation gauge data to whole watersheds (i.e. basins).}
\usage{
ms_calc_watershed_precip(
  precip,
  ws_boundary,
  precip_gauge,
  pchem,
  parallel = TRUE,
  maxcores = Inf,
  out_path,
  elevation_agnostic = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{precip}{optional \code{data.frame} or path to a directory containing feather files.
File or directory must represent a dataset in MacroSheds format (see \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262}{MacroSheds EDI} for definitions),
containing precipitation depth time series. Expects daily data in mm. A \code{tibble} of precip depth in mm, in
MacroSheds format, can be generated by \code{\link[=ms_load_product]{ms_load_product()}}. If \code{precip} is
supplied and \code{pchem} is not, only watershed-average precip is returned.
If both are supplied, both are returned, as well as watershed-average precipitation chemical flux,
computed from the cell-wise product of precip and pchem after imputation}

\item{ws_boundary}{\code{sf} object or path to a directory \emph{containing} directories, each in turn containing individual shapefiles
of watershed boundaries. Object/files must have site_code and geometry features (columns).
See \code{\link[=ms_delineate_watershed]{ms_delineate_watershed()}}.}

\item{precip_gauge}{\code{sf} object or path to a directory \emph{containing} directories, each in turn containing shapefiles
of precipitation gauge locations. Object/files must have site_code and geometry features (columns).
All supplied precip gauges will be used for imputation, even if they are far from
the watershed of interest.}

\item{pchem}{optional \code{data.frame} or path to a directory containing feather files.
File or directory must represent a dataset in MacroSheds format (see \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262}{MacroSheds EDI} for definitions),
containing precipitation chemistry time series. Expects daily data in mg/L. See \code{\link[=ms_synchronize_timestep]{ms_synchronize_timestep()}}
and \code{\link[=ms_conversions]{ms_conversions()}} to make these modifications. A
\code{tibble} of precip concentration, in
MacroSheds format, can be generated by \code{\link[=ms_load_product]{ms_load_product()}}. Note that some default units of MacroSheds
variables are not mg/L, and you may need \code{\link[=ms_conversions]{ms_conversions()}} even if you've acquired data via \code{\link[=ms_load_product]{ms_load_product()}}.
If \code{pchem} is
supplied and \code{precip} is not, only watershed-average pchem is returned.
If both are supplied, both are returned, as well as watershed-average precipitation chemical flux,
computed from the cell-wise product of precip and pchem after imputation.}

\item{parallel}{logical. If TRUE (default), interpolation attempts to use \code{maxcores} CPU cores/threads.}

\item{maxcores}{numeric. Default \code{Inf}; the maximum number of cores (or hyperthreads)
used in parallel processing.
Ignored if \code{parallel} is FALSE. If greater than the available number of cores/hyperthreads,
all available cores/hyperthreads will be used.}

\item{out_path}{character. Directory to which output data will be saved.}

\item{elevation_agnostic}{Logical. Should elevation data be ignored when interpolating
precipitation data? TRUE means "ignore elevation." See details for more information.}

\item{verbose}{Logical. If TRUE (default), prints more information during run.}
}
\value{
Saves watershed-average precipitation depth and/or chemistry, and chemical flux (if both precip
and pchem are supplied) to \code{out_path} as feather files in MacroSheds format (see \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262}{MacroSheds EDI}).
Output units for flux are kg/d. See \code{\link[=ms_scale_flux_by_area]{ms_scale_flux_by_area()}} to convert to kg/ha/d.
}
\description{
Using precipitation gauge recordings (depth and/or chemistry) and locations,
this function interpolates an estimated value at each cell of an automatically retrieved digital
elevation model (DEM), then averages cell values to generate a watershed average
at every time step. The local relationship between
elevation and precipitation depth may also be leveraged for better prediction.
If precipitation chemistry is supplied in addition to depth, then watershed-average
precipitation depth, chemistry, and chemical flux estimates will all be generated.
}
\details{
This function uses inverse distance weighted interpolation to extend gauge measurements
to every cell of a watershed DEM. If \code{elevation_agnostic} is set to TRUE, the resulting
watershed average will be a simple arithmetic mean of all watershed cell values, ignoring
the local relationship between precipitation and elevation.
If FALSE (the default), the resulting watershed average will be a weighted mean of two separate estimates: the
mean of cell values (weight: 1) \emph{and} the prediction of a linear regression with elevation (weight: absolute value of adjusted R^2 of modeled data).
At least 3 gauges must record at a given time step for the regression to be leveraged in generating the final prediction.
Note that these computations are performed at every time step, using all available
(non-missing, non-NA) gauges.

This function will interpolate only precipitation depth if no
pchem is supplied. It will only interpolate precipitation chemistry
if no precip data is supplied. If both precip and pchem are supplied, then
precipitation depth, chemical concentrations, and precipitation chemical fluxes
will all be calculated.

In general, elevation is a relevant predictor for precipitation depth, but not necessarily
concentration or flux of precip chemical components. As such, \code{elevation_agnostic}
is always set to TRUE (ignore the elev-precip relationship) when determining watershed-average precipitation chemistry \emph{in the case of both}
\code{precip} and \code{pchem} being supplied. If only \code{pchem} or \code{precip}is
supplied and \code{elevation_agnostic} is FALSE, then the elevation-pchem relationship
\emph{will} be incorporated into the final estimate of the watershed average
for each time step.

Note that this function does not return the imputed watershed grid for each
timestep--only the watershed-average estimate(s) as tibbles.
}
\examples{
See vignette: https://github.com/MacroSHEDS/macrosheds/blob/master/vignettes/ms_interpolate_precip.md
}
\seealso{
\code{\link[=ms_load_product]{ms_load_product()}}, \code{\link[=ms_delineate_watershed]{ms_delineate_watershed()}}, \code{\link[=ms_scale_flux_by_area]{ms_scale_flux_by_area()}}
}
\author{
Spencer Rhea

Mike Vlah, \email{vlahm13@gmail.com}

Wes Slaughter
}
