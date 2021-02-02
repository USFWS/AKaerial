#' Current table of guild associations for relevant species on the WBPHS survey
#'
#' Current table of all species on the USFWS Region 11 Migratory Bird Management component of the North American (WBPHS) aerial survey
#' and their guild (for eider, grebe, merganser, and scoter spp.) associations.
#'
#'
#' @docType data
#'
#' @usage data(WBPHS_PoolSpecies)
#'
#' @format A data frame consisting of 48 rows of 4 variables:
#' \describe{
#'   \item{SPECIES}{The 4 character code representing species.}
#'   \item{COMMON}{The accepted common name of the species represented.}
#'   \item{SCIENTIFIC}{The accepted scientific name of the species represented.}
#'   \item{GUILD}{Character string indicating what (if any) guild the species belongs to.  Possible values include:
#'     \describe{
#'     \item{Eider}{Common, spectacled, king, or Steller's eider.}
#'     \item{Grebe}{Red-necked or horned grebe.}
#'     \item{Merganser}{Common or red-breasted merganser.}
#'     \item{Scoter}{Black, white-winged, or surf scoter.}
#'     }
#'   }
#' }
#'
#'
"WBPHS_PoolSpecies"
