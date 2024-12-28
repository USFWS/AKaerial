
#' Display a combination of strata, transect design, and data
#'
#' ObsMap will take input layers of strata, transect design, and data and display them.
#'
#' ObsMap is designed to take a directory path for a combination of stratification base layer,
#' aerial transect design, and resulting data and display it using an interactive map.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The project (ACP, CRD, YKD, YKG) designation, takes the place of strata and transect,
#' and when combined with year, also chooses the transect layer and data files.
#' @param strata The path name to the .gpkg file of the stratification.
#' @param transect The path name to the .gpkg file of lines designating the transect design.
#' @param data The path name(s) to the data file(s).
#' @param year The year to be displayed.
#' @param observer The observer initials to be displayed.
#' @param species The species code(s) to be displayed.
#'
#' @return Map object
#'
#' @examples
#'  ObsMap(area="CRD", year=2024, species="DCGO")
#'
#' @export
ObsMap=function(area="none", strata, transect, data, year, observer="both", species="all"){

if(area != "none"){
entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]
}

if(area=="YKG"){area="YKD"}

if(area=="ACP"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data/source_data/ACP_DesignStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/incoming/data/source_data/ACP_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data"
  if(length(entries$OBS) == 2){
    obs=rbind(
    read.csv(paste(root, entries$OBS[1], sep="")),
    read.csv(paste(root, entries$OBS[2], sep=""))
    )
  }
  if(length(entries$OBS) == 1){
    obs=read.csv(paste(root, entries$OBS[1], sep=""))
  }
}


if(area=="CRD"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data/source_data/CRD_DesignStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data/source_data/CRD_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data"
  if(length(entries$OBS) == 2){
    obs=rbind(
      read.csv(paste(root, entries$OBS[1], sep="")),
      read.csv(paste(root, entries$OBS[2], sep=""))
    )
  }
  if(length(entries$OBS) == 1){
    obs=read.csv(paste(root, entries$OBS[1], sep=""))
  }
  }

if(area=="YKD"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data/source_data/YKG_AnalysisStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data/source_data/YKD_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data"
  if(length(entries$OBS) == 2){
    obs=rbind(
      read.csv(paste(root, entries$OBS[1], sep="")),
      read.csv(paste(root, entries$OBS[2], sep=""))
    )
  }
  if(length(entries$OBS) == 1){
    obs=read.csv(paste(root, entries$OBS[1], sep=""))
  }
  }

if(observer != "both"){obs = obs %>% filter(Observer==observer)}
if(species != "all"){obs = obs %>% filter(Species %in% species)}

obs = st_as_sf(obs, coords = c("Lon","Lat"))

st_crs(obs) = 4269

ggplot() +
  geom_sf(data=strata, aes(fill=STRATNAME)) +
  geom_sf(data = transects) +
  geom_sf(data = obs)

}
