% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{SpeciesTransect}
\alias{SpeciesTransect}
\title{Provide transect-level summaries for a given species}
\usage{
SpeciesTransect(
  area,
  year,
  species,
  strata.overwrite = "none",
  method = "repo"
)
}
\arguments{
\item{area}{The area code for dedicated MBM Alaska region surveys.
  Acceptable values include:
\itemize{
\item YKD - Yukon Kuskokwim Delta, ducks
\item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
\item YKG - Yukon Kuskokwim Delta, geese
\item ACP - Arctic Coastal Plain
\item CRD - Copper River Delta
}}

\item{year}{The range of years}

\item{species}{The (usually 4 character) species code of the desired species}

\item{strata.overwrite}{forced overwrite of stratification information}
}
\value{
List object with 3 elements: \enumerate{
\item output.table summary table of species occurrence
\item M.table summary of stratification characteristics
\item output.flkdrake flkdrake-specific information
}
}
\description{
SepciesTransect will summarize spatial and observation information for a range of years by Obs_Type
}
\details{
SpeciesTransect is used to provide a full record of a species on an area for a range of years, by transect and strata.
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
