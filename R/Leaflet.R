
BLSC_data=read.csv("Q:/Waterfowl/BLSC Survey/Data/QC Transcribed Data/BLSC_2018_QCObs_ALL.csv", header=TRUE, stringsAsFactors = F)

BLSC_strata=LoadMap(map="Q:/Waterfowl/BLSC Survey/Design Files/Design Strata/BLSC_2018_DesignStrata.shp",
                    lay="BLSC_2018_DesignStrata")

coordinates(BLSC_data) = ~Lon+Lat

leaflet() %>%
  addTiles() %>%
  addPolygons(data=BLSC,
              fillColor=~factpal(BLSC$STRATNAME),
              fillOpacity=.6,
              stroke=TRUE,
              color="black",
              opacity=1,
              weight=1,
              popup = paste("Strata: ", BLSC$STRATNAME)) %>%
  addPolylines(data=trans.proj,
               color="black",
               weight=4,
               opacity=.9,
               label=~trans.proj$id,
               popup = paste("Strata: ", BLSC$STRATNAME, "<br>",
                             "Old Transect: ", trans.proj$ORIGID, "<br>",
                             "New Transect: ", trans.proj$id))  %>%

  addCircleMarkers(data=BLSC_data,
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5,
    popup= paste(BLSC_data$Observer, "<br>", BLSC_data$Species, "<br>", BLSC_data$Time)
    ) %>%

  addScaleBar()


