

#' Format and summarize a clean data set for analysis
#'
#' DataProcess will combine observation, design transect, and stratification layers into a summary list for analysis
#'
#' DataProcess reads in a clean data file (passed through GreenLight function) and pulls all observations with code==1.  All observations
#' coded as open 1 are then changed to single 1.  The transect layer is then placed on the stratification and trimmed and renumbered from
#' navigational numbers to analysis numbering.  Species codes that need specific project treatments will be changed.  Counts are adjusted
#' for different indices at the row level. The resulting list object is the primary currency for design-based ratio estimator analysis.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param data.path The location of the QA/QC checked data file
#' @param strata.path The location of the analysis stratification .gpkg file
#' @param transect.path The location of the transect design .gpkg file
#' @param threshold The distance in kilometers from a design file where observations are no longer counted.  Defaults to 0.5 km.
#' @param strata.id The identifier of the stratification naming
#' @param trans.id The identifier of the correct transect numbering
#' @param transect.layer The name of the design transect layer in the geopackage
#'
#' @return List object with 5 elements: \enumerate{
#' \item obs The original data file with corrected transect and stratification information
#' \item flight The flown transects for the observer with corrected numbering
#' \item design The split design information
#' \item strata The spatial polygon summary for the stratification layer
#' \item transect Observations summarized at the transect level
#' }
#'
#' @export
DataProcess <- function(area,
                        data.path=NA,
                        strata.path=NA,
                        transect.path=NA,
                        threshold=.5,
                        strata.id="STRATNAME",
                        trans.id="OBJECTID",
                        transect.layer,
                        retain="liberal"){


  if(area=="YKDV" || area=="KIG"){area="YKD"}

  data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)

  data=data[data$Code==1,]

  data$Obs_Type[data$Obs_Type=="open" & data$Num==1 & data$Species!="SWANN"]="single"

  data=AssignStrata(full.data=data, strata.id=strata.id, strata.file=strata.path, retain=retain)

  design.trans = sf::st_read(transect.path, layer=transect.layer, quiet=TRUE)

  colnames(design.trans)[colnames(design.trans)=="ORIGID"]="Transect"

  #data = merge(x=data, y=sf::st_drop_geometry(design.trans[,c("Transect", trans.id)]), by="Transect")

  data$DesignTransect=NA

  if((area == "YKG") & (data$Year[1] < 1999)){
      data=AssignTransect(full.data=data, transect.path=transect.path, transect.layer=transect.layer, trans.id=trans.id)
      data$DesignTransect=data$closest}

  if((area != "YKG") | (data$Year[1] >= 1999)){

    for(i in 1:length(data$DesignTransect)){

     if(data$Transect[i] %in% unique(unlist(sf::st_drop_geometry(design.trans[,"Transect"])))){

    data$DesignTransect[i]=unlist(sf::st_drop_geometry(design.trans[design.trans$Transect==data$Transect[i],trans.id]))
            }

    if(!(data$Transect[i] %in% unique(unlist(sf::st_drop_geometry(design.trans[,"Transect"]))))){
          data$DesignTransect[i]=NA}
  }}


  data=DistanceToDesign(full.data=data, design.trans=design.trans, trans.id=trans.id)

  #colnames(data)[colnames(data)==trans.id]="DesignTransect"

  data=data %>% filter(Distance <= (threshold * 1000))

  data$ctran = paste(data$DesignTransect, data$Stratum, sep=" ")

  design=TransSummarySF(transect.file=transect.path,
                        transect.layer=transect.layer,
                        strata.file=strata.path,
                        strata.id=strata.id,
                        trans.id=trans.id)

  flight = sf::st_drop_geometry(design) %>%
    dplyr::mutate(Year=unique(data$Year)[1], Observer=unique(data$Observer)[1], Seat=unique(data$Seat)[1])
  #  dplyr::select(-Shape)

  colnames(flight)[colnames(flight)==strata.id]="Strata"
  colnames(flight)[colnames(flight)==trans.id]="Original"
  colnames(flight)[colnames(flight)=="TRANSTRAT"]="PartOf"
  colnames(flight)[colnames(flight)=="LENGTH"]="Length"

  flown.list=unique(data$ctran)

  flight = flight %>%
    dplyr::filter(ctran %in% flown.list)

  data=SpeciesByProject(full.data=data, area=area)

  data=AdjustCounts(data)

  data=AddClass(data)

  data = data %>%
    dplyr::mutate(Lon= sf::st_coordinates(.)[,1],
                  Lat= sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry(data)

  strata=StrataSummarySF(strata.file=strata.path, id=strata.id)

  data=list("obs"=data, "flight"=flight, "design"=design, "strata"=strata)

  if(area=="YKG"){data=FixTavs(selected.data=data)}

  data$obs$ctran=as.character(data$obs$ctran)

  transect.summary=TransDataSF(data, strata.id=strata.id)

  data$transect=transect.summary


  return(data)

}
