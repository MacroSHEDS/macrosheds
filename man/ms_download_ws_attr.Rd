% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_download_ws_attr.R
\name{ms_download_ws_attr}
\alias{ms_download_ws_attr}
\title{Download MacroSheds watershed attributes}
\usage{
ms_download_ws_attr(
  macrosheds_root,
  dataset = "summaries",
  quiet = FALSE,
  version = "latest",
  omit_climate_data = FALSE,
  skip_existing = TRUE,
  timeout = 300
)
}
\arguments{
\item{macrosheds_root}{character. Directory where watershed attribute files will be downloaded.
If this directory does not exist, it will be created. Does not have to be the same
as \code{macrosheds_root} provided to \code{\link[=ms_download_core_data]{ms_download_core_data()}}, but might as well be. This directory should
only be used for MacroSheds data files.}

\item{dataset}{character. This function can download each of the four collections of watershed attribute data
provided by MacroSheds.
\itemize{
\item "summaries": watershed attributes summarized across time and space (i.e. one value for each site).
\item "time series": temporally explicit watershed attributes where available. See \code{omit_climate_data} parameter.
\item "CAMELS summaries" and...
\item "CAMELS Daymet forcings": additional watershed
summary data that conform as closely as possible to the specifications of the
\href{https://ral.ucar.edu/solutions/products/camels}{CAMELS dataset}. See MacroSheds metadata
for a list of discrepancies.
\item you may also use "all" to retrieve all four datasets. \code{omit_climate_data} will still be recognized.
}

Once downloaded, data can be loaded into R with \code{\link[=ms_load_product]{ms_load_product()}}.}

\item{quiet}{logical. If TRUE, some messages will be suppressed.}

\item{version}{numeric or "latest". The MacroSheds dataset version to download.}

\item{omit_climate_data}{logical. Ignored unless \code{dataset == 'time series'}. If you don't care about climate data,
you may use this argument to avoid downloading it (because it's large, approx 2 GiB), while still downloading
terrain, vegetation, parent material, land use, and hydrology data.}

\item{skip_existing}{logical. If FALSE, dataset components already downloaded to macrosheds_root will
be overwritten. If TRUE, these will be skipped.}

\item{timeout}{integer. Temporarily overrides \code{getOption(timeout)}.}
}
\value{
Returns NULL. Downloads watershed attribute data to the
directory specified by \code{macrosheds_root}. For full documentation, visit
\href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1262}{EDI}.
}
\description{
Download MacroSheds watershed attribute data for all sites. Choose between
temporally explicit spatial summaries, or summaries across both time and space.
For some variables that are not time-varying, these are the same.
}
\examples{
ms_download_ws_attr(macrosheds_root = 'data/macrosheds', dataset = 'time series')
}
\seealso{
\code{\link[=ms_download_core_data]{ms_download_core_data()}}, \code{\link[=ms_load_product]{ms_load_product()}}
}
\author{
Wes Slaughter, \email{wslaughter@berkeley.edu}

Mike Vlah, \email{vlahm13@gmail.com}

Spencer Rhea
}
