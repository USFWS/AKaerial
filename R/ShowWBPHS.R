
#' Display observations on WBPHS segments
#'
#' ShowWBPHS will load a leaflet display of WBPHS design segments and observations
#'
#' ShowWBPHS provides a quick spatial overview of a set of observations and the Waterfowl Breeding Population Habitat Survey segments.
#' I am unsure of the origin and accuracy of the segments.  Segments and observations are drawn using leaflet.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data WBPHS clean (greenlighted) observations.
#' @param trans.file Directory path to the WBPHS segment shape file (default to Q:/Waterfowl/WBPHS/Design Files/Design_Transects/NAWBPS_segments.shp)
#'
#' @return Interactive map of segment-level data
#'
#' @export
ShowWBPHS=function(data, trans.file="Q:/Waterfowl/WBPHS/Design Files/Design_Transects/NAWBPS_segments.shp"){

  trans.layer=file_path_sans_ext(basename(trans.file))

  trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
  trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

  pal=colorFactor(palette = c("red", "green", "yellow", "blue", "orange"), trans.proj$Segment)
  pal2=colorFactor(palette = c("red", "yellow"), data$Observer)

  map= leaflet() %>%
    addTiles() %>%

    addPolylines(data=trans.proj,
                 color=~pal(Segment),
                 weight=4,
                 opacity=.9,
                 label=~trans.proj$Seq,
                 popup = paste("Strata: ", trans.proj$Seq, "<br>",
                               "Transect: ", trans.proj$Transect, "<br>",
                               "Segment: ", trans.proj$Segment, "<br>"))  %>%

    addProviderTiles("Esri.WorldImagery") %>%

    addCircleMarkers(data=data,
                     radius = 3,
                     color = ~pal2(Observer),
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     popup= paste(data$Observer, "<br>", data$Species,
                                  "<br>", data$Obs_Type, "<br>", data$Num)
    ) %>%


    addScaleBar()

  print(map)
}

