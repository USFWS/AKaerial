#' Create the big beautiful file for AK Waterfowl data
#'
#' Create the big beautiful file for AK Waterfowl data and save it to a new .rda
#'
#' This function updates the big beautiful file with new data found in MasterObs, MasterStrata, and MasterEffort.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @return The associated object is updated and saved in the data folder of the package.
#'
#' @export
BBFcreate = function(){

  colnames(MasterStrata)[1]="Stratum"
  colnames(MasterEffort)[1]="Stratum"

  bbf = left_join(MasterObs, MasterStrata)
  bbf = left_join(bbf, MasterEffort)

  save(bbf, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/bbf.rda")

}
