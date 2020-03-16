
#' Display a combination of strata, transect design, and data
#'
#' ShowMe will take input layers of strata, transect design, and data and display them using Leaflet.
#'
#' ShowMe is designed to take a directory path for a combination of stratification base layer,
#' aerial transect design, and resulting data and display it using an interactive Leaflet map.
#' Data can be more than one file, and can be filtered to a single species or group of species. Clicking on strata will bring up
#' the STRATNAME field associated with the click.  Clicking on a transect will display the
#' associated strata identification, transect numbering, recalculated (SPLIT) transect number
#' (see \code{\link{SplitDesign}}), and length in kilometers. Clicking an observation will display
#' the observer initials, species code, observation type, number of the type observed, and the
#' transect number \emph{reported by the pilot or observer.}
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param transect.path The path name to the .shp file of lines designating the transect design.
#' Must be in a format accepted by \code{\link{SplitDesign}}.
#' @param data.path The path name(s) to the data file(s).  At minimum, data file must have columns
#' Observer, Species, Obs_Type, Num, and Transect.
#' @param species The species code(s) to be displayed.
#'    Acceptable values are those in \code{sppntable}.
#'
#' @return None
#'
#' @examples
#'  ShowMe(strata.path="C:/Habitat.shp", transect.path="My2016Transects.shp", data.path="My2016obs.csv", species=c("SPEI", "STEI"))
#'
#' @export
ShowMe=function(strata.path, transect.path, data.path, species="all", bounds=NA, bounds.which=NA){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- sp::spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addPolylines(data=split.design,
                 color="black",
                 weight=4,
                 opacity=.9,
                 label=~split.design$OBJECTID,
                 popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                               "Old Transect: ", split.design$ORIGID, "<br>",
                               "New Transect: ", split.design$OBJECTID, "<br>",
                               "Split Transect: ", split.design$SPLIT, "<br>",
                               "Length: ", split.design$len))  %>%

    leaflet::addScaleBar()

  for(i in 1:length(data.path)){

    temp.data=read.csv(data.path[i], header=TRUE, stringsAsFactors = FALSE)

    if(i==1){
      data=temp.data
    }

    if(i!=1){
      data=rbind(data, temp.data)

    }

    if(species != "all"){
      data=data[data$Species %in% species, ]

    }

  }

  sp::coordinates(data)=~Lon+Lat

  pal=leaflet::colorFactor(palette = c("red", "blue", "yellow", "green","orange"), data$Observer)


  map= map %>%

    leaflet::addCircleMarkers(data=data,
                     radius = 3,
                     color = ~pal(Observer),
                     stroke = FALSE,
                     fillOpacity = 1,
                     popup= paste(data$Observer, "<br>", data$Species,
                                  "<br>", data$Obs_Type, "<br>", data$Num,
                                  "<br>", data$Transect)
                  ) %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  if(bounds.which>0){


    map = map %>%
      fitBounds(bounds[[bounds.which]]$coords[1], bounds[[bounds.which]]$coords[2], bounds[[bounds.which]]$coords[3], bounds[[bounds.which]]$coords[4])

  }

  print(map)



}



#' Display a combination of strata and transect design
#'
#' ShowMeDesign will take input layers of strata and transect design and display them using Leaflet.
#'
#' ShowMeDesign is designed to take a directory path for a combination of stratification base layer,
#' aerial transect design and display it using an interactive Leaflet map.
#' Clicking on strata will bring up the STRATNAME field associated with the click.
#' Clicking on a transect will display the
#' associated strata identification, transect numbering, recalculated (SPLIT) transect number
#' (see \code{\link{SplitDesign}}), and length in kilometers.  Similar to \code{\link{ShowMe}},
#' but with no option to include observations.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param transect.path The path name to the .shp file of lines designating the transect design.
#' Must be in a format accepted by \code{\link{SplitDesign}}.
#'
#' @return None
#'
#' @examples
#'  ShowMeDesign(strata.path="C:/Habitat.shp", transect.path="My2016Transects.shp")
#'
#' @export
ShowMeDesign=function(strata.path, transect.path){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- sp::spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addPolylines(data=split.design,
                 color="black",
                 weight=4,
                 opacity=.9,
                 label=~split.design$OBJECTID,
                 popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                               "Old Transect: ", split.design$ORIGID, "<br>",
                               "New Transect: ", split.design$OBJECTID, "<br>",
                               "Split Transect: ", split.design$SPLIT, "<br>",
                               "Length: ", split.design$len))  %>%

    leaflet::addScaleBar() %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  print(map)



}




#' Display a combination of unprocessed strata and transect design
#'
#' ShowMeUncut will take input layers of strata and transect design display them (unprocessed) using Leaflet.
#'
#' ShowMeUncut is designed to take a directory path for a combination of stratification base layer,
#' aerial transect design and display them using an interactive Leaflet map.
#' Clicking on strata will bring up the STRATNAME field associated with the click.
#' Clicking on a transect will display the associated strata identification and transect numbering.
#' This is different than other ShowMe functions in that it uses unprocessed transect files that
#' haven't been applied and clipped to the associated strata layer.  Transects will still be displayed
#' if they fall outside strata boundaries.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.
#' @param transect.path The path name to the .shp file of lines designating the transect design.
#' @param data.path The path name(s) to the data file(s).  At minimum, data file must have columns
#' Observer, Species, Obs_Type, and Num.
#'
#' @return None
#'
#' @examples
#'  ShowMeUncut(strata.path="C:/Habitat.shp", transect.path="My2016Transects.shp")
#'
#' @export
ShowMeUncut=function(strata.path, transect.path, data.path="none"){


  strata.proj=LoadMap(strata.path, type="proj")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))
  design=rgdal::readOGR(transect.path, layer=tools::file_path_sans_ext(basename(transect.path)), verbose=FALSE)
  design.proj <- sp::spTransform(design, "+proj=longlat +ellps=WGS84")
  design.proj <- smoothr::drop_crumbs(design.proj, threshold=.1)


  factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addPolylines(data=design.proj,
                 color="white",
                 weight=4,
                 opacity=.9,
                 label=~design.proj$OBJECTID,
                 popup = paste("Strata: ", design.proj$STRATNAME, "<br>",
                               "Old Transect: ", design.proj$ORIGID, "<br>",
                               "New Transect: ", design.proj$OBJECTID, "<br>"
                               ))  %>%

    leaflet::addScaleBar() %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  if(data.path != "none"){

  for(i in 1:length(data.path)){

    temp.data=read.csv(data.path[i], header=TRUE, stringsAsFactors = FALSE)

    if(i==1){
      data=temp.data
    }

    if(i!=1){
      data=rbind(data, temp.data)

    }


    data$Lat[is.na(data$Lat)]=1
    data$Lon[is.na(data$Lon)]=1

  }



  sp::coordinates(data)=~Lon+Lat

  pal=leaflet::colorFactor(palette = c("red", "blue", "yellow", "green","orange"), data$Observer)


  map= map %>%

    leaflet::addCircleMarkers(data=data,
                     radius = 3,
                     color = ~pal(Observer),
                     stroke = FALSE,
                     fillOpacity = 0.9,
                     popup= paste(data$Observer, "<br>", data$Species,
                                  "<br>", data$Obs_Type, "<br>", data$Num)
    ) %>%

    leaflet::addProviderTiles("Esri.WorldImagery")

  }




  print(map)



}




#' Display multiple years of observation data by survey area
#'
#' ShowMeYears will take input data for an area and range of years and display them using Leaflet.
#'
#' ShowMeYears is designed to take a combination of area and year(s) and look up associated files
#' in the \code{MasterFileList} to display the exact stratification and data that
#' would be used to generate index estimates using an interactive Leaflet map.
#' Data can be filtered to a single species or group of species. Clicking on strata will bring up
#' the STRATNAME field associated with the click. Clicking an observation will display
#' the observer initials, species code, observation type, number of the type observed,
#' transect number \emph{reported by the pilot or observer.}, and year of the observation.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta MBM duck stratification
#'  \item YKDV - Yukon Kuskokwim Delta VCF study duck stratification
#'  \item YKG - Yukon Kuskokwim Delta MBM goose stratification
#'  \item KIG - Kigigak Island only
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  }
#' @param year The year or range of years to display.
#' @param species The species code(s) to be displayed.
#'    Acceptable values are those in \code{sppntable}.
#'
#' @return None
#'
#' @examples
#'  ShowMeYears(area="ACP", year=c(2015:2019), species=c("SPEI", "STEI"))
#'
#' @export
ShowMeYears=function(area, year, species="all"){

  area2="NotBarrow"

  if(area=="Barrow"){
    area2="Barrow"
    area="ACP"}


  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]
  strata.path=paste(entries$DRIVE[1], entries$STRATA[1], sep="")


  if(area2=="Barrow"){strata.path="T:/OSNAS/ACP_CROPPED.shp"}


  data.path=c()

  for (n in 1:length(entries$OBS)){

    data.path[n]=paste(entries$DRIVE[n], entries$OBS[n], sep="")

  }



  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addScaleBar()

  for(i in 1:length(data.path)){

    temp.data=read.csv(data.path[i], header=TRUE, stringsAsFactors = FALSE)

    if(i==1){
      data=temp.data
    }

    if(i!=1){
      data=rbind(data, temp.data)

    }

    if(species[1] != "all"){
      data=data[data$Species %in% species, ]

    }

  }

  sp::coordinates(data)=~Lon+Lat

  pal=leaflet::colorFactor(palette = c("white", "blue", "yellow", "green","orange"), data$Species)


  map= map %>%

    leaflet::addCircleMarkers(data=data,
                     radius = 3,
                     color = ~pal(Species),
                     stroke = FALSE,
                     fillOpacity = 1,
                     popup= paste(data$Observer, "<br>", data$Species,
                                  "<br>", data$Obs_Type, "<br>", "n = ",data$Num,
                                  "<br>", "Transect ", data$Transect, "<br>", data$Year)
    ) %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  print(map)
  return(map)


}




#' Display a combination of strata, transect design, and actual track flown
#'
#' ShowMeTrackDesign will take input layers of strata and transect design and display them using Leaflet with actual track flown overlay.
#'
#' ShowMeTrackDesign is designed to take a directory path for a combination of stratification base layer,
#' aerial transect design, and actual track flown and display it using an interactive Leaflet map.
#' Clicking on strata will bring up the STRATNAME field associated with the click.
#' Clicking on a transect will display the
#' associated strata identification, transect numbering, recalculated (SPLIT) transect number
#' (see \code{\link{SplitDesign}}), and length in kilometers.  Similar to \code{\link{ShowMe}},
#' but with no option to include observations.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param transect.path The path name to the .shp file of lines designating the transect design.
#' Must be in a format accepted by \code{\link{SplitDesign}}.
#' @param track.path The path name to the \emph{folder} containing the track files.
#' Track files must be .txt extension and contain \strong{lat, long, time, date.}
#' @param observer The observer initials to be used for track file selection.  Track files must
#' follow standard naming convention for track file objects to select observers correctly.
#' @param years The years of track files to display.
#'
#' @return None
#'
#' @examples
#'  ShowMeTrackDesign(strata.path="C:/Habitat.shp", transect.path="My2016Transects.shp", track.path="c:/track files/", observer="CJF", years=2019)
#'
#' @export
ShowMeTrackDesign=function(strata.path, transect.path, track.path, observer, years){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- sp::spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))

  #get all track files

  file.set=list.files(track.path)

  #remove anything that isn't txt

  file.set=file.set[tools::file_ext(file.set)=="txt"]

  track=c()

  for(i in 1:length(file.set)){

  obs=tools::file_path_sans_ext(strsplit(file.set[1],"_")[[1]][4])

  if(obs==observer){

    data=read.csv(paste(track.path,file.set[i], sep=""), header=FALSE, stringsAsFactors = FALSE)

    data$date=strsplit(file.set[i],"_")[[1]][2]

    track=rbind(track, data)

  }

  }

  colnames(track)=c("lat", "long", "time", "date")

  track$lat=as.numeric(track$lat)
  track$long=as.numeric(track$long)

  track=track[complete.cases(track),]

  track$year=as.numeric(substr(track$date, 1, 4))

  track=track[track$year %in% years, ]

  sp::coordinates(track)=~long+lat


  paths <- sp::split(track, track$date)

  sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "line1")))

  for (p in 2:length(paths)) {
    id <- paste0("line", as.character(p))
    l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)))
    sp_lines <- maptools::spRbind(sp_lines, l)
  }




factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data=strata.proj,
                         fillColor=~factpal(strata.proj$STRATNAME),
                         fillOpacity=.4,
                         stroke=TRUE,
                         color="black",
                         opacity=1,
                         weight=1,
                         popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addPolylines(data=split.design,
                          color="green",
                          weight=4,
                          opacity=.9,
                          label=~split.design$OBJECTID,
                          popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                                        "Old Transect: ", split.design$ORIGID, "<br>",
                                        "New Transect: ", split.design$OBJECTID, "<br>",
                                        "Split Transect: ", split.design$SPLIT, "<br>",
                                        "Length: ", split.design$len))  %>%

    leaflet::addPolylines(data=sp_lines,
                          color="red",
                          weight=4,
                          opacity=.9) %>%


    leaflet::addScaleBar() %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  print(map)



}

