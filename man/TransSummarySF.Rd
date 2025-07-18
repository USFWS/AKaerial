% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TransSummarySF.R
\name{TransSummarySF}
\alias{TransSummarySF}
\title{Summarize design transects for analysis using sf package}
\usage{
TransSummarySF(
  transect.file,
  transect.layer,
  strata.file,
  strata.layer,
  trans.id = "OBJECTID",
  strata.id = "STRATNAME",
  current = "Transect"
)
}
\arguments{
\item{transect.file}{The path to the design stratification file}

\item{transect.layer}{The name of the design transect layer in the geopackage}

\item{strata.file}{The path to the analysis stratification layer}

\item{strata.layer}{The strata gpkg layer}

\item{trans.id}{The identifier of the correct transect numbering}

\item{strata.id}{The identifier of the stratification layer}

\item{current}{The identifier of the current navigational transect}
}
\value{
data frame summary of sampled transects
}
\description{
TransSummarySF will provide the overall design transect spatial characteristics for analysis
}
\details{
TransSummarySF will compute sampled lengths (in km) and areas of each transect (in km^2) in a design transect file
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
