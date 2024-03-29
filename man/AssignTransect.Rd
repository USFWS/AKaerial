% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AssignTransect.R
\name{AssignTransect}
\alias{AssignTransect}
\title{Assign transect ID to data for analysis using sf package}
\usage{
AssignTransect(full.data, transect.path, trans.id, transect.layer)
}
\arguments{
\item{full.data}{The data set of georeferenced observations}

\item{transect.path}{The path to the design transect geopackage}

\item{trans.id}{The identifier of the naming column in the design file}

\item{transect.layer}{The layer referenced in the geopackage}
}
\value{
data frame of observations with modified Transect column
}
\description{
AssignTransect will provide the closest transect associated with a point in a clean data set
}
\details{
AssignTransect will calculate the nearest design transect to observations in a data file and overwrite the
Transect column in the data with the nearest LineString trans.id
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
