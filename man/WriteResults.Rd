% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{WriteResults}
\alias{WriteResults}
\title{Output .csv files of historic estimates}
\usage{
WriteResults(area, year = "all", species = "all", output.folder)
}
\arguments{
\item{area}{The area code for dedicated MBM Alaska region surveys.
  Acceptable values include:
\itemize{
\item YKD - Yukon Kuskokwim Delta, ducks
\item YKG - Yukon Kuskokwim Delta, geese
\item ACP - Arctic Coastal Plain
\item CRD - Copper River Delta
\item YKDV - Yukon Kuskokwim Delta, ducks, visibility study strata
}}

\item{year}{The years of estimates to add}

\item{species}{The species requested (defaults to all)}

\item{output.folder}{The folder path for the resulting 3 .csv files}
}
\value{
The tables are generated and written to the output folder.
}
\description{
Output a range of annual estimates for a given aerial survey
}
\details{
This function will output .csv files representing estimates from the chosen survey, year(s), and species.
}
\references{
\url{https://github.com/USFWS/AKaerial}
}
\author{
Charles Frost, \email{charles_frost@fws.gov}
}
