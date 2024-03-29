% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greenlight.R
\name{SeatCheck}
\alias{SeatCheck}
\title{Check if Seat entries are all correct}
\usage{
SeatCheck(data)
}
\arguments{
\item{col.data}{The column to be checked.}
}
\value{
data frame of TRUE/FALSE and offending entries
}
\description{
SeatCheck checks to see if Seat entries are correct values needed for analysis.
}
\details{
SeatCheck is a GreenLight QA/QC check performed on a data set.  If the Seat entries are not LF, RF, LR, or RR, it will return a "red" status.
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
