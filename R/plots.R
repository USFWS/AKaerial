

ShowMe=function(strata.path, data.path, transect.path){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(gBuffer(strata.proj, byid=TRUE, width=0))


  factpal=colorFactor(brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet() %>%
    addTiles() %>%
    addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    addPolylines(data=split.design,
                 color="black",
                 weight=4,
                 opacity=.9,
                 label=~split.design$OBJECTID,
                 popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                               "Old Transect: ", split.design$ORIGID, "<br>",
                               "New Transect: ", split.design$OBJECTID, "<br>",
                               "Split Transect: ", split.design$SPLIT, "<br>",
                               "Length: ", split.design$len))  %>%

    addScaleBar()

  for(i in 1:length(data.path)){

    temp.data=read.csv(data.path[i], header=TRUE, stringsAsFactors = FALSE)

    if(i==1){
      data=temp.data
    }

    if(i!=1){
      data=rbind(data, temp.data)

    }

  }

  coordinates(data)=~Lon+Lat

  pal=colorFactor(palette = c("red", "blue", "yellow", "green","orange"), data$Observer)


  map= map %>%

    addCircleMarkers(data=data,
                     radius = 3,
                     color = ~pal(Observer),
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     popup= paste(data$Observer, "<br>", data$Species,
                                  "<br>", data$Obs_Type, "<br>", data$Num)
                  ) %>%

    addProviderTiles("Esri.WorldImagery")


  print(map)



}





ShowMeDesign=function(strata.path, transect.path){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(gBuffer(strata.proj, byid=TRUE, width=0))


  factpal=colorFactor(brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


  map= leaflet() %>%
    addTiles() %>%
    addPolygons(data=strata.proj,
                fillColor=~factpal(strata.proj$STRATNAME),
                fillOpacity=.4,
                stroke=TRUE,
                color="black",
                opacity=1,
                weight=1,
                popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

    addPolylines(data=split.design,
                 color="black",
                 weight=4,
                 opacity=.9,
                 label=~split.design$OBJECTID,
                 popup = paste("Strata: ", split.design$STRATNAME, "<br>",
                               "Old Transect: ", split.design$ORIGID, "<br>",
                               "New Transect: ", split.design$OBJECTID, "<br>",
                               "Split Transect: ", split.design$SPLIT, "<br>",
                               "Length: ", split.design$len))  %>%

    addScaleBar() %>%

    addProviderTiles("Esri.WorldImagery")


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

