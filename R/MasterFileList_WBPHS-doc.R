#' Current table of archived input file names and locations for USFWS Region 11 Migratory Bird Management portion of the North American (WBPHS) aerial survey
#'
#' Current table of archived input file names and locations for USFWS Region 11 Migratory Bird Management portion of the
#' North American (Waterfowl Breeding Population and Habitat Survey, WBPHS, BPOP) aerial survey.
#' For other MBM survey files, see \code{\link[AKaerial]{MasterFileList}}.
#' This survey consists of low-level aerial observations of waterfowl on a stratified strip transect design.
#' For more information on how estimates are generated, see \code{\link[AKaerial]{ReadWBPHS}} or \code{\link[AKaerial]{EstimatesWBPHS}}.
#'
#'
#' @docType data
#'
#' @usage data(MasterFileList_WBPHS)
#'
#' @format A data frame consisting of 136 rows of 4 variables:
#' \describe{
#'   \item{YEAR}{The 4-digit integer year of the survey.}
#'   \item{DRIVE}{The directory drive where the files are located.  Included to facilitate changes to directory structure.}
#'   \item{OBS}{The complete directory path (without drive letter) to the YEAR-specific QCObs file (see \code{\link[AKaerial]{GreenLight}}) of transcribed observations.}
#'   \item{AREA}{Character string indicating what study area files refer to (WBPHS).}
#' }
#'
#'
"MasterFileList_WBPHS"
