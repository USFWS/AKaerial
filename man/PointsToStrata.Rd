% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{PointsToStrata}
\alias{PointsToStrata}
\title{Sample the polygon under an observation}
\usage{
PointsToStrata(full.data, area)
}
\arguments{
\item{full.data}{A data frame of observations.}

\item{area}{The project area}
}
\value{
data frame with Strata column overwritten
}
\description{
PointsToStrata will attribute each observation in a file to underlying strata
}
\details{
PointsToStrata will sample the polygon layer under each observation and replace the Strata column entry with the appropriate strata.
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
