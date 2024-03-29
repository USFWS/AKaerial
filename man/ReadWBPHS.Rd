% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NorthAmerican.R
\name{ReadWBPHS}
\alias{ReadWBPHS}
\title{Prepare WBPHS data for index estimate calculation}
\usage{
ReadWBPHS(full.data)
}
\arguments{
\item{full.data}{An R data frame of observations in a given year of the survey}
}
\value{
The same data frame with additional index columns and modified counts, as well as modified Stratum 9 observations.
}
\description{
Load and adjust counts for WBPHS data to prepare for index estimate calculation
}
\details{
This function takes standard greenlight data for the WBPHS (Waterfowl Breeding Population Habitat Survey, or North American) and prepares it for
index estimate calculations.  The function calls Cut9 to remove observations in Stratum 9 by a pre-defined longitudinal gradient for swans,
cackling Canada geese, and greater white-fronted geese.  These observations are removed prior to index estimation since they are included in
other MBM coastal zone surveys (YKG).  This function also adjusts the counts for each of 4 indices:
\enumerate{
\item itotal - Indicated total.  Singles doubled, pairs doubled, opens added, flkdrake 1-4 doubled, flkdrake 5+ added.
\item ibb - Indicated breeding birds.  Singles doubled, pairs doubled, opens removed, flkdrake 1-4 doubled, flkdrake 5+ removed.
\item total - Total birds.  Singles added, pairs doubled, opens added, flkdrake added.
\item sing1pair2 - Singles and pairs.  Singles added, pairs doubled, opens removed, flkdrake removed.
\item flock - Flocks.  Opens and flkdrakes of 5 or more.
}
In addition, due to inconsistencies in interpretation of the field protocol for data collection, open 1 and open 2 are changed to single 1
and pair 1, respectively, across the entire data set.
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
