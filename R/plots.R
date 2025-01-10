
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
#' @references \url{https://github.com/USFWS/AKaerial}
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


  if(!is.na(bounds.which) & bounds.which>0){


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
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param transect.path The path name to the .shp file of lines designating the transect design.
#' Must be in a format accepted by \code{\link{SplitDesign}}.
#' @param area The area for the prject represented.  This only matters for the area CRD where there are vertical transects
#' to be passed to SplitDesign.
#'
#' @return None
#'
#' @examples
#'  ShowMeDesign(strata.path="C:/Habitat.shp", transect.path="My2016Transects.shp")
#'
#' @export
ShowMeDesign=function(strata.path, transect.path, area="Not CRD"){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE, area=area)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- sp::spTransform(split.design, "+proj=longlat +ellps=WGS84 +datum=NAD83")
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
                 weight=2,
                 opacity=.9,
                 label=~split.design$OBJECTID,
                 popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                               "Old Transect: ", split.design$ORIGID, "<br>",
                               "New Transect: ", split.design$OBJECTID, "<br>",
                               "Split Transect: ", split.design$SPLIT, "<br>",
                               "Length: ", split.design$len))  %>%

    leaflet::addScaleBar() %>%

    leaflet::addProviderTiles("Esri.WorldImagery") %>%

    addMouseCoordinates()


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
#' @references \url{https://github.com/USFWS/AKaerial}
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
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta MBM duck stratification
#'  \item YKG - Yukon Kuskokwim Delta MBM goose stratification
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

    if(species[1] == "all"){
      data=data[data$Species != "START", ]
      data=data[data$Species != "END", ]


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
#' @references \url{https://github.com/USFWS/AKaerial}
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





#' Create a basic exportable table from historic estimates, including n-year average
#'
#' ReportTable will summarize historic estimates from in an .html table that can be exported for reporting.
#'
#' ReportTable will take one of the historic estimates tables (package data for AKaerial) and display it as an .html table
#' object suitable for an external report.  The user can specify n for an n-year average if one is desired.  A similarly
#' structured data frame could be used in place of one of the historic estimates tables, provided it has columns for Year,
#' Species, an index, and that index variance.  Table caption and column headings can either be specified as arguments or
#' entered by the user once the table is created
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The estimates object.  Must be in a format with Year, Species, index, and index variance if not one of
#' the included package data objects (ACPHistoric, CRDHistoric, YKDHistoric, YKDVHistoric, or YKGHistoric).
#' @param species The species chosen (see \code{\link{sppntable}} for options).  Currently
#' supports no more than 1 species.
#' @param year The range of years for the table.
#' @param index The column names that specify the index estimate and its variance.
#' Must be ordered correctly (index, variance) or (index1, variance1, index2, variance2).  Currently up to 2 indices are supported.
#' If no index is specified, the user will be prompted with choices from the data.
#' @param yr.avg The number of years in the n-year running average requested.
#' @param cap The overall table caption.  If nothing entered as an argument, the user will be prompted to enter a caption in the function.
#' @param new.names The desired column names for the final table.  If these are not specified the user will be prompted to
#' input within the function.
#' @param missing Any missing years of data to be displayed as NA
#'
#' @return Renders an .html table and also returns it in data frame format
#'
#' @examples ReportTable(data=ACPHistoric$combined, species="SPEI", year = c(2007:2019), index = c("total", "total.var"), yr.avg=3, cap="Test table!", new.names=c("Year", "Total Birds", "SE", "3-year Avg", "SE"))
#'
#' @export
ReportTable= function(data, species, year, index="none", yr.avg, cap="none", new.names="none", missing=NULL){

  library(dplyr)

  data=as.data.frame.list(data)

  always_keep = c("Year", "Observer", "Species", "strata")

  if(index[1] == "none"){
  print(colnames(data))
  message("Which index or indices?")
  message("Split multiples with a comma (total.est,var.N)")
  message("MAKE SURE TO INCLUDE THE VARIANCE COLUMN")
  index <- readline(prompt=" ")
  index = strsplit(index, ",")
  index = unlist(index)
  }


  data = data %>%

    select((starts_with(index) & ends_with(index)) | contains(always_keep)) %>%

    filter(Species %in% species & Year %in% year) %>%

    mutate_at(c(1:length(index)), ~round(., 0))

  if("strata" %in% colnames(data)){data = data %>% relocate("strata")}
  if("Species" %in% colnames(data)){data = data %>% select(-"Species")}
  if("Observer" %in% colnames(data)){data = data %>% relocate("Observer")}
  if("Year" %in% colnames(data)){data = data %>% relocate("Year")}


  if(yr.avg == 0 & length(index) == 2){

    data[,which(colnames(data)==index[2])]=round(sqrt(data[,which(colnames(data)==index[2])]),0)

  }

  if(yr.avg == 0 & length(index) > 2){

    data[,which(colnames(data)==index[2])]=round(sqrt(data[,which(colnames(data)==index[2])]),0)
    data[,which(colnames(data)==index[4])]=round(sqrt(data[,which(colnames(data)==index[4])]),0)


  }

  if(yr.avg > 1 & length(index) > 1){

    temp = data %>%

      select(starts_with(index[1]) & ends_with(index[1])) %>%

      mutate(avg1=round(zoo::rollapply(.,yr.avg,mean,align='right',fill=NA),0))



    temp2 = data %>%

      select(starts_with(index[2]) & ends_with(index[2])) %>%

      mutate(se1=round(zoo::rollapply(.,yr.avg,function(x) sqrt(sum(x[-yr.avg]/yr.avg^2)),align='right',fill=NA),0))


    data = data %>%

      mutate(avg1 = temp[2]) %>%

      mutate(se1 = temp2[2]) %>%

      relocate(avg1, .after=index[2]) %>%

      relocate(se1, .after=avg1)

    data[,which(colnames(data)==index[2])]=round(sqrt(data[,which(colnames(data)==index[2])]),0)

  }

  if(yr.avg > 1 & length(index) > 2){

    temp = data %>%

      select(starts_with(index[3]) & ends_with(index[3])) %>%

      mutate(avg1=round(zoo::rollapply(.,yr.avg,mean,align='right',fill=NA),0))


    temp2 = data %>%

      select(starts_with(index[4]) & ends_with(index[4])) %>%

      mutate(se1=round(zoo::rollapply(.,yr.avg,function(x) sqrt(sum(x[-yr.avg]/yr.avg^2)),align='right',fill=NA),0))

    data = data %>%

      mutate(avg2 = temp[2]) %>%

      mutate(se2 = temp2[2])


    data[,which(colnames(data)==index[4])]=round(sqrt(data[,which(colnames(data)==index[4])]),0)


    }


  if(new.names[1] == "none"){
  message("Current data structure:")

  print(head(data))

  message("Specify column names separated by a comma (,)")

  new.names <- readline(prompt=" ")
  new.names = strsplit(new.names, ",")
  new.names = unlist(new.names)
  }

  colnames(data)=new.names


  if(cap == "none"){
  message("Specify table caption:")
  cap <- readline(prompt=" ")
  }



  if(missing){
    for(i in 1:length(missing)){

      data = data %>%
        add_row(Year=missing[i], .after = which(data$Year==missing[i]-1))
    }

  }

data$Year=as.character(data$Year)

  data %>%


    kableExtra::kable(format="latex",
                      format.args = list(big.mark = ","),
          escape = F,
          col.names = new.names,

          caption = cap) %>%

    kableExtra::row_spec(0,bold=TRUE) %>%


    kableExtra::kable_styling(full_width=FALSE)

}





#' Create a basic figure from historic estimates
#'
#' ReportFigure will summarize historic estimates from Alaska Region aerial surveys in a point graph that can be exported for reporting.
#'
#' ReportFigure will take one of the historic estimates tables (package data for AKaerial) and display it as a ggplot figure
#' suitable for an external report.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area Which area (project) will be plotted (ACP, CRD, YKD, YKG)
#' @param species The species chosen (see \code{\link{sppntable}} for options).
#' @param year The range of years for the figure.
#' @param type Observer-specific (observer), strata-specific (strata), or combined (combined; default value)
#' @param index The population index requested (total, itotal, ibb, sing1pair2, flock)
#' @param index2 The second index to be plotted simultaneously (optional).
#' @param title The overall figure title.  If nothing entered as an argument, the user will be prompted to enter a title in the function.
#' @param x.title The desired x axis label for the final figure.
#' @param y.title The desired y axis label for the final figure.
#'
#' @return Renders and returns a ggplot figure
#'
#' @examples ReportFigure(area="ACP",
#' species=c("MALL", "SPEI"),
#' index="ibb",
#' index2="itotal",
#' title="Mallard and Spectacled Eider population indices from the Arctic Coastal Plain, 2007-2024")
#'
#' @export
ReportFigure= function(area,
                       species,
                       year="all",
                       type="combined",
                       index,
                       index2="none",
                       title="",
                       x.title="Year",
                       y.title="Population Index"
                       ){


  library(ggplot2)

  if(area=="ACP" & type=="combined"){
    data=ACPHistoric$combined
    data$Observer="Combined"}
  if(area=="ACP" & type=="observer"){
    data=ACPHistoric$output.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }
  if(area=="ACP" & type=="strata"){
    data=ACPHistoric$expanded.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }

  if(area=="YKD" & type=="combined"){
    data=YKDHistoric$combined
    data$Observer="Combined"}
  if(area=="YKD" & type=="observer"){
    data=YKDHistoric$output.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }
  if(area=="YKD" & type=="strata"){
    data=YKDHistoric$expanded.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }

  if(area=="YKG" & type=="combined"){
    data=YKGHistoric$combined
  data$Observer="Combined"}
  if(area=="YKG" & type=="observer"){
    data=YKGHistoric$output.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }
  if(area=="YKG" & type=="strata"){
  data=YKGHistoric$expanded.table
  colnames(data)[colnames(data)=="total.est"]="total"
  colnames(data)[colnames(data)=="var.N"]="total.var"
  data$total.se=sqrt(data$total.var)
  colnames(data)[colnames(data)=="itotal.est"]="itotal"
  colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
  data$itotal.se=sqrt(data$itotal.var)
  colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
  colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
  data$ibb.se=sqrt(data$ibb.var)
  colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
  colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
  data$sing1pair2.se=sqrt(data$sing1pair2.var)
  colnames(data)[colnames(data)=="flock.est"]="flock"
  colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
  data$flock.se=sqrt(data$flock.var)
  }

  if(area=="CRD" & type=="combined"){
    data=CRDHistoric$combined
    data$Observer="Combined"}
  if(area=="CRD" & type=="observer"){
    data=CRDHistoric$output.table
  colnames(data)[colnames(data)=="total.est"]="total"
  colnames(data)[colnames(data)=="var.N"]="total.var"
  data$total.se=sqrt(data$total.var)
  colnames(data)[colnames(data)=="itotal.est"]="itotal"
  colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
  data$itotal.se=sqrt(data$itotal.var)
  colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
  colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
  data$ibb.se=sqrt(data$ibb.var)
  colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
  colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
  data$sing1pair2.se=sqrt(data$sing1pair2.var)
  colnames(data)[colnames(data)=="flock.est"]="flock"
  colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
  data$flock.se=sqrt(data$flock.var)
  }
  if(area=="CRD" & type=="strata"){
    data=CRDHistoric$expanded.table
    colnames(data)[colnames(data)=="total.est"]="total"
    colnames(data)[colnames(data)=="var.N"]="total.var"
    data$total.se=sqrt(data$total.var)
    colnames(data)[colnames(data)=="itotal.est"]="itotal"
    colnames(data)[colnames(data)=="var.Ni"]="itotal.var"
    data$itotal.se=sqrt(data$itotal.var)
    colnames(data)[colnames(data)=="ibbtotal.est"]="ibb"
    colnames(data)[colnames(data)=="var.Nib"]="ibb.var"
    data$ibb.se=sqrt(data$ibb.var)
    colnames(data)[colnames(data)=="sing1pair2.est"]="sing1pair2"
    colnames(data)[colnames(data)=="var.Nsing1pair2"]="sing1pair2.var"
    data$sing1pair2.se=sqrt(data$sing1pair2.var)
    colnames(data)[colnames(data)=="flock.est"]="flock"
    colnames(data)[colnames(data)=="var.Nflock"]="flock.var"
    data$flock.se=sqrt(data$flock.var)
    }

  if(year=="all"){year=unique(data$Year)}

  data = data %>% filter((Year %in% year) & (Species %in% species))


  if(index2=="none"){

  if(index=="total"){data1=data %>% select(Year, Species, Observer, total, total.se)}
  if(index=="itotal"){data1=data %>% select(Year, Species, Observer, itotal, itotal.se)}
  if(index=="ibb"){data1=data %>% select(Year, Species, Observer, ibb, ibb.se)}
  if(index=="sing1pair2"){data1=data %>% select(Year, Species, Observer, sing1pair2, sing1pair2.se)}
  if(index=="flock"){data1=data %>% select(Year, Species, Observer, flock, flock.se)}

  colnames(data1)[4] = "index"
  colnames(data1)[5] = "se"


  plot1 = ggplot(data=data1, (aes(x=Year, y=index))) +
    ggtitle(title) +
    scale_x_continuous(name=x.title, limits=c(min(data$Year-1), max(data$Year+1)), breaks=seq(min(data$Year),max(data$Year),5), expand = c(0, 0)) +
    scale_y_continuous(label=scales::comma, name=y.title, expand = c(0.1, 0)) +
    geom_point(data=data1, aes(Year, index), color="black", fill="black") +
    geom_point(data=data1, aes(Year, index)) +

    geom_pointrange(aes(x=Year, y=index, ymin=pmax(0, index-1.96*se), ymax=index+1.96*se),data = data1)
  }


  if(index2 != "none"){

    if(index=="total"){data1=data %>% select(Year, Species, Observer, total, total.se) %>% mutate(Index="Total")}
    if(index=="itotal"){data1=data %>% select(Year, Species, Observer, itotal, itotal.se) %>% mutate(Index="Indicated Total")}
    if(index=="ibb"){data1=data %>% select(Year, Species, Observer, ibb, ibb.se) %>% mutate(Index="Indicated Breeding")}
    if(index=="sing1pair2"){data1=data %>% select(Year, Species, Observer, sing1pair2, sing1pair2.se) %>% mutate(Index="Breeding Birds")}
    if(index=="flock"){data1=data %>% select(Year, Species, Observer, flock, flock.se) %>% mutate(Index="Flocked Birds")}



    if(index2=="total"){data2=data %>% select(Year, Species, Observer, total, total.se) %>% mutate(Index="Total")}
    if(index2=="itotal"){data2=data %>% select(Year, Species, Observer, itotal, itotal.se) %>% mutate(Index="Indicated Total")}
    if(index2=="ibb"){data2=data %>% select(Year, Species, Observer, ibb, ibb.se) %>% mutate(Index="Indicated Breeding")}
    if(index2=="sing1pair2"){data2=data %>% select(Year, Species, Observer, sing1pair2, sing1pair2.se) %>% mutate(Index="Breeding Birds")}
    if(index2=="flock"){data2=data %>% select(Year, Species, Observer, flock, flock.se) %>% mutate(Index="Flocked Birds")}

    colnames(data1)[4] = "index"
    colnames(data1)[5] = "se"
    colnames(data2)[4] = "index"
    colnames(data2)[5] = "se"


    data3 = rbind(data1, data2)

    plot1 = ggplot(data=data3, (aes(x=Year, y=index))) +
      ggtitle(title) +
      scale_x_continuous(name=x.title, limits=c(min(data$Year-1), max(data$Year+1)), breaks=seq(min(data$Year),max(data$Year),5), expand = c(0, 0)) +
      scale_y_continuous(label=scales::comma, name=y.title, expand = c(0.1, 0)) +
      geom_point(data=data3, aes(Year, index, color=forcats::fct_inorder(Index)), position=position_dodge(.3)) +
      geom_pointrange(aes(x=Year, y=index, ymin=pmax(0, index-1.96*se), ymax=index+1.96*se, color=forcats::fct_inorder(Index)),position=position_dodge(.3), data = data3) +
      scale_color_discrete(type=c("black", "darkgray"))
  }


 plot1=plot1 +
   theme_bw() +
   theme(legend.position = "top", legend.title=element_blank())


  print(plot1)

}
