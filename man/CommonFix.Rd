% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greenlight.R
\name{CommonFix}
\alias{CommonFix}
\title{Apply "yellow light" changes to a data set}
\usage{
CommonFix(data, fix, area)
}
\arguments{
\item{data}{The data frame to be fixed.}

\item{fix}{The character string vector of the names of the fixes to be applied.}

\item{area}{The project area designation.}
}
\value{
data frame of fixed columns
}
\description{
CommonFix is used in conjunction with GreenLight to apply "no-loss" changes to a data set
}
\details{
Commonfix will take "yellow light" issues and apply a known treatment to fix offending issues. The list of fixes includes \itemize{
  \item Seat - changes lower to uppercase, flips the characters (FR, FL, RL to RF, LF, LR)
  \item Observer - changes lower to uppercase
  \item Swan - breaks up a nest-only observation into 2 observations
  \item Obs_Type - changes open 2 or open 1 SWAN to pair or single, changes SWANN to open 1
  \item Species - changes incorrect or outdated species codes to current ones}
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
