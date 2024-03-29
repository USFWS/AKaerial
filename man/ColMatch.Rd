% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greenlight.R
\name{ColMatch}
\alias{ColMatch}
\title{Check if column names are appropriate for analysis.}
\usage{
ColMatch(data, necessary)
}
\arguments{
\item{data}{The data frame to be checked.}
}
\value{
TRUE/FALSE
}
\description{
ColMatch checks to see if the column headings match those needed by AKAerial for analysis.
}
\details{
ColMatch is the first GreenLight QA/QC check performed on a data set.  If the column names do not match the necessary set of names in GreenLight,
the function will throw an error and end abruptly.  For the current list of acceptable names, see \code{\link{GreenLight}}
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
