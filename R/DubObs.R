



#' Derive a random double observer sampling design
#'
#' DoubleObsRandom will load an existing stratified transect design and assign a third
#' observer approximately equally by strata and area randomly behind the pilot or observer
#'
#' DoubleObsRandom is designed to take 2 spatial objects: a stratification base layer
#' and transect layer represented as spatial lines, and assign a third observer (during
#' a double-observer detection study) approximately equally behind both front seat observers.
#' The function simulates possible random combinations of left and right seat and will throw
#' out those that aren't within \code{split} proportion.  The resulting map and design will
#' offer n options (\code{options} defaults to 5) that can then be checked by the field crew
#' for logistical feasibility.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param solution The solution from a \code{\link{DoubleObsRandom}} optimization.
#'
#' @return None
#'
#' @examples
#'  ShowMeDouble(strata.path="C:/Habitat.shp", solution=DoubleObsRandom(strata=MyStrata, transects=MyTransects))
#'
#' @export
DoubleObsRandom=function(strata, transects, split=.4, options=5, tries=10000){

  split.design=SplitDesign(strata.file = strata, transect.file = transects, SegCheck = FALSE)

  trans.list=unique(split.design$SPLIT)

  how.many=ceiling(length(trans.list)/2)

  possible=vector(mode="list", length=options)
  possible.t=vector(mode="list", length=options)

  for(i in 1:options){

    for(j in 1:tries){

      cat(paste("\r", j, " of ",tries, sep="" ))

      split.design$newseat="LR"

      sample=sample(split.design$SPLIT, how.many, replace=FALSE)

      split.design$newseat[split.design$SPLIT %in% sample]="RR"

      coverage=aggregate(len~STRATNAME+newseat,data=split.design, FUN=sum)

      prop.coverage=aggregate(len~STRATNAME, data=coverage, FUN=function(x) x[1]/sum(x))

      prop.coverage=prop.coverage[prop.coverage$STRATNAME != "nonhabitat",]

      if(!any(prop.coverage$len > (1-split) | prop.coverage$len < split))
      {
        print(paste("Solution ", i, " found.", sep=""))

        possible[[i]]=split.design
        possible.t[[i]]=prop.coverage

        break

      }

    }



  }



  return(list("map"=possible,
              "table"=possible.t))


}





#' Display a \code{\link{DoubleObsRandom}} solution
#'
#' ShowMeDouble will display the solution found by \code{\link{DoubleObsRandom}} on
#' the associated strata using Leaflet.
#'
#' ShowMeDouble is designed to take a directory path for a stratification base layer
#' and the result of \code{\link{DoubleObsRandom}} and produce an interactive Leaflet map.
#' Clicking on strata will bring up the STRATNAME field associated with the click.
#' Clicking on a transect will display the associated strata identification, transect numbering,
#' recalculated (SPLIT) transect number (see \code{\link{SplitDesign}}), and
#' optimal positioning of the third observer (behind pilot or front-seat observer).
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path name to the .shp file of the stratification.  Must be in a format
#' accepted by \code{\link{SplitDesign}}.
#' @param solution The solution from a \code{\link{DoubleObsRandom}} optimization.
#'
#' @return Returns a leaflet object for display.
#'
#' @examples
#'  ShowMeDouble(strata.path="C:/Habitat.shp", solution=DoubleObsRandom(strata=MyStrata, transects=MyTransects))
#'
#' @export
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




