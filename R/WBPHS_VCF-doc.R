#' Current table of VCFs (visibility correction factors) for relevant species on the WBPHS survey
#'
#' Current table of all species on the USFWS Region 11 Migratory Bird Management component of the North American (WBPHS) aerial survey
#' and their associated visibility correction factors (VCFs).
#'
#'
#' @docType data
#'
#' @usage data(WBPHS_VCF)
#'
#' @format A data frame consisting of 48 rows of 4 variables:
#' \describe{
#'   \item{SPECIES}{The 4 character code representing species.}
#'   \item{COMMON}{The accepted common name of the species represented.}
#'   \item{SCIENTIFIC}{The accepted scientific name of the species represented.}
#'   \item{STRATUM}{The WBPHS stratum that the VCF applies to.}
#'   \item{VCF}{Floating point decimal visibility correction factor; represents the number of birds missed for every bird seen on the survey.}
#'   \item{VCF_SE}{Floating point decimal standard error of the associated VCF.}
#'   }
#' }
#'
#'
"WBPHS_VCF"
