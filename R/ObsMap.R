
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
#' @param type Static (default) or dynamic map.
#'
#' @return Map object
#'
#' @examples
#'  ObsMap(area="CRD", year=2024, species="DCGO")
#'
#' @export
ObsMap=function(area="none", strata, transect, data, year=0, observer="both", species="all", type="static"){

if(year[1]==0){year=max(MasterFileList$YEAR)}

if(area != "none"){
entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]
}

if(area=="YKG"){area="YKD"}

if(area=="ACP"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data/source_data/ACP_DesignStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data/source_data/ACP_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data"
  # if(length(entries$OBS) == 2){
  #   obs=rbind(
  #   read.csv(paste(root, entries$OBS[1], sep="")),
  #   read.csv(paste(root, entries$OBS[2], sep=""))
  #   )
  # }
  # if(length(entries$OBS) == 1){
  #   obs=read.csv(paste(root, entries$OBS[1], sep=""))
  # }

  for(k in 1:length(entries$OBS)){
    obs = read.csv(paste(root, entries$OBS[k], sep=""))
    if(k==1){full.obs=obs}
    if(k!=1){full.obs=rbind(full.obs, obs)}

  }

}


if(area=="CRD"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data/source_data/CRD_DesignStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data/source_data/CRD_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data"
  for(k in 1:length(entries$OBS)){
    obs = read.csv(paste(root, entries$OBS[k], sep=""))
    if(k==1){full.obs=obs}
    if(k!=1){full.obs=rbind(full.obs, obs)}

  }
  }

if(area=="YKD"){
  strata = read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data/source_data/YKG_AnalysisStrata.gpkg")
  transects=read_sf("//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data/source_data/YKD_DesignTrans.gpkg", layer=entries$LAYER[1])
  root="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data"
  for(k in 1:length(entries$OBS)){
    obs = read.csv(paste(root, entries$OBS[k], sep=""))
    if(k==1){full.obs=obs}
    if(k!=1){full.obs=rbind(full.obs, obs)}

  }
  }

if(observer != "both"){full.obs = full.obs %>% filter(Observer==observer)}
if(species != "all"){full.obs = full.obs %>% filter(Species %in% species)}

full.obs = st_as_sf(full.obs, coords = c("Lon","Lat"))

st_crs(full.obs) = 4269


if(type=="static"){
m = ggplot2::ggplot() +
  ggplot2::geom_sf(data=strata, ggplot2::aes(fill=STRATNAME), alpha=.5) +
  ggplot2::geom_sf(data = transects) +
  ggplot2::geom_sf(data = full.obs) +
  ggplot2::labs(fill = "Stratum") +
  ggplot2::theme(legend.position="top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 panel.border = element_rect(colour = "black", fill=NA, linewidth=1))



}

if(type=="dynamic"){

  tmap::tmap_mode(mode=c("view"))
  m <- tmap::tm_shape(strata) + tmap::tm_polygons(col = "STRATNAME", alpha = 0.5, title="Stratum") +
    tmap::tm_shape(transects) + tmap::tm_lines() +
    tmap::tm_shape(full.obs) + tmap::tm_dots() +
    tmap::tm_basemap(server = "Esri.WorldGrayCanvas") +
    tmap::tm_scale_bar()



}

return(m)

}
