% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MasterFileList-doc.R
\docType{data}
\name{MasterFileList}
\alias{MasterFileList}
\title{Current table of archived input file names and locations for USFWS Region 11 Migratory Bird Management aerial surveys}
\format{
A data frame consisting of 264 rows of 7 variables:
\describe{
  \item{YEAR}{The 4-digit integer year of the survey.}
  \item{DRIVE}{The directory drive where the files are located.  Included to facilitate changes to directory structure.}
  \item{STRATA}{The complete directory path (without drive letter) to the YEAR- and AREA-specific ESRI .shp file representing the study area boundaries and strata polygons.}
  \item{TRANS}{The complete directory path (without drive letter) to the YEAR- and AREA-specific ESRI .shp file representing the design transects.}
  \item{OBS}{The complete directory path (without drive letter) to the YEAR-specific QCObs file (see \code{\link[AKaerial]{GreenLight}}) of transcribed observations.}
  \item{AREA}{Character string indicating what study area files refer to.  Possible values include:
    \describe{
    \item{ACP}{Arctic Coastal Plain}
    \item{A1002}{Arctic 10-02}
    \item{CRD}{Copper River Delta}
    \item{KIG}{Kigigak Island subset of YKD}
    \item{YKD}{Yukon-Kuskokwim River Delta (Duck)}
    \item{YKDV}{Yukon-Kuskokwim River Delta (Duck) VCF Study}
    \item{YKG}{Yukon-Kuskokwim River Delta (Goose)}
    }
  }
  \item{COMBINE}{Binary [0,1] indicating whether multiple observer files are combined into 1 file before analysis.}
}
}
\usage{
data(MasterFileList)
}
\description{
Current table of archived input file names and locations for USFWS Region 11 Migratory Bird Management aerial surveys, including
ACP, A1002, CRD, KIG, YKD, YKDV, and YKG.  For WBPHS files, see \code{\link[AKaerial]{MasterFileList_WBPHS}}.
These surveys consist of low-level aerial observations of waterfowl on stratified strip transect designs.
For more information on how estimates are generated, see \code{\link[AKaerial]{DataSelect}} or \code{\link[AKaerial]{Densities}}.
}
\keyword{datasets}
