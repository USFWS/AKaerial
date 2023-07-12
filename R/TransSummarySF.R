#' Summarize design transects for analysis using sf package
#'
#' TransSummarySF will provide the overall design transect spatial characteristics for analysis
#'
#' TransSummarySF will compute sampled lengths (in km) and areas of each transect (in km^2) in a design transect file
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param transect.file The path to the design stratification file
#' @param transect.layer The name of the design transect layer in the geopackage
#' @param strata.file The path to the analysis stratification layer
#' @param trans.id The identifier of the correct transect numbering
#' @param strata.id The identifier of the stratification layer
#' @param current The identifier of the current navigational transect
#'
#' @return data frame summary of sampled transects
#'
#' @export
TransSummarySF=function(transect.file,
                        transect.layer,
                        strata.file,
                        trans.id="OBJECTID",
                        strata.id="STRATNAME",
                        current="Transect"){

  design.trans = sf::st_read(transect.file, layer=transect.layer, quiet=TRUE) %>%
    sf::st_transform(4269)

  analysis.strata = sf::st_read(dsn=strata.file, quiet=TRUE) %>%
    sf::st_transform(4269)



  newlines=st_intersection(design.trans, analysis.strata)

  tsum= newlines %>%
    group_by(get(strata.id), get(trans.id)) %>%
    summarise(LENGTH=units::set_units(st_length(st_union(.)), km))

  tsum$LENGTH=units::set_units(st_length(tsum), km)

  tsum$SampledArea = units::set_units(.2, km) * tsum$LENGTH

  colnames(tsum)[colnames(tsum)=="get(strata.id)"]=strata.id
  colnames(tsum)[colnames(tsum)=="get(trans.id)"]=trans.id

  tsum$ctran=NA

  for(i in 1:length(tsum$SampledArea)){
  tsum$ctran[i] = paste(tsum[i,trans.id], tsum[i,strata.id], sep=" ")
  }

  return(tsum)
}


