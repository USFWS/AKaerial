#' Aggregate observations by Obs_Type, corrected transect, species, observer, and year
#'
#' TransDataSF will provide an aggregated transect-level summary of all observations in a DataSelect output list by observer and year
#'
#' TransDataSF is designed to take an object from DataSelect and summarize observations at the transect level.  Each row is a unique
#' combination of year, observer, species, transect, and Obs_Type.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param selected.data The data object from DataSelect
#'
#' @return data frame with transect-level summary of observations
#'
#' @export
TransDataSF=function(selected.data, strata.id){

  selected.data$obs$Obs_Type[selected.data$obs$Obs_Type=="flkdrake" & selected.data$obs$Num<5] = "flkdrake4"
  selected.data$obs$Obs_Type[selected.data$obs$Obs_Type=="flkdrake" & selected.data$obs$Num>4] = "flkdrake5"

  transect.level= selected.data$obs %>%
    dplyr::group_by(Year, Observer, Species, Obs_Type, ctran) %>%
    dplyr::summarise(
      Num=sum(Num),
      total=sum(total),
      itotal=sum(itotal),
      ibb=sum(ibb),
      sing1pair2=sum(sing1pair2),
      flock=sum(flock)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(., Year, Observer, Species, Obs_Type, ctran, fill=list(Num=0, itotal=0, total=0, ibb=0, sing1pair2=0, flock=0))

  transect.level= merge(x=transect.level, y=sf::st_drop_geometry(selected.data$design[,c("ctran", "SampledArea", strata.id)]), by="ctran", all.x=TRUE)

colnames(transect.level)[colnames(transect.level)==strata.id]="strata"
colnames(transect.level)[colnames(transect.level)=="SampledArea"]="area"

transect.level = transect.level %>%
  select(Year, Observer, Species, Obs_Type, ctran, Num, itotal, total, ibb, sing1pair2, flock, area, strata) %>%
  arrange(Species, ctran, Obs_Type)


return(transect.level)

}
