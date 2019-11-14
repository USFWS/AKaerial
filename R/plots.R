

ShowMe=function(strata.path, data.path, transect.path, species="all"){


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


  print(map)



}





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




ShowMeDouble=function(strata.path, solution){
  strata.proj=LoadMap(strata.path, type="proj")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))



  factpal=leaflet::colorFactor(RColorBrewer::brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))
  pal=leaflet::colorFactor(palette = c("red", "blue"), solution$newseat)

  map= leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
    leaflet::addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    leaflet::addPolylines(data=solution,
                 color=~pal(solution$newseat),
                 weight=4,
                 opacity=.9,
                 label=~solution$OBJECTID,
                 popup = paste("Strata: ", solution$STRATNAME, "<br>",
                               "Old Transect: ", solution$ORIGID, "<br>",
                               "New Transect: ", solution$OBJECTID, "<br>",
                               "Split Transect: ", solution$SPLIT, "<br>",
                               "Seat: ", solution$newseat))  %>%

    leaflet::addScaleBar() %>%

    leaflet::addProviderTiles("Esri.WorldImagery")


  print(map)

return(map)

}




ShowMeUncut=function(strata.path, transect.path, data.path="none"){


  strata.proj=LoadMap(strata.path, type="proj")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))
  design=rgdal::readOGR(transect.path, layer=tools::file_path_sans_ext(basename(transect.path)), verbose=FALSE)
  design.proj <- sp::spTransform(design, "+proj=longlat +ellps=WGS84")
  design.proj <- smoothr::drop_crumbs(design.proj, threshold=.1)


  factpal=leaflet::colorFactor(brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


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
                               "New Transect: ", design.proj$OBJECTID, "<br>",
                               "Split Transect: ", design.proj$SPLIT, "<br>",
                               "Length: ", design.proj$len))  %>%

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




ShowMeYears=function(area, year, species="all"){

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]
  strata.path=paste(entries$DRIVE[1], entries$STRATA[1], sep="")

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



