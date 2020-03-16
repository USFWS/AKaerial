



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






#' Match observations for double observer surveys
#'
#' DubMatch attempts to match observations between 2 observers on a survey by time and group size.
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
DubMatch = function(front.data=NA, rear.data=NA, combined.data=NA, front.obs, rear.obs, time=6, open=5){

  #check if combined data was given, if not, read in each file

  if(class(front.data)=="data.frame"){

    f=front.data[front.data$obs==front.obs, ]

  }

  if(class(rear.data)=="data.frame"){

    r=rear.data[rear.data$obs==rear.obs, ]


  }

  if(class(front.data)=="character"){

    data=read.csv(front.data, header=TRUE, stringsAsFactors = FALSE)
    f=data[data$obs==front.obs, ]


  }

  if(class(rear.data)=="character"){

    data=read.csv(rear.data, header=TRUE, stringsAsFactors = FALSE)
    r=data[data$obs==rear.obs, ]


  }

  if(class(combined.data)=="character"){

    data=read.csv(combined.data, header=TRUE, stringsAsFactors = FALSE)
    f=data[data$obs==front.obs, ]
    r=data[data$obs==rear.obs, ]

  }

  if(class(combined.data)=="data.frame"){

    f=combined.data[combined.data$obs==front.obs, ]
    r=combined.data[combined.data$obs==rear.obs, ]


  }

  f$matched=0
  r$matched=0


  f.tran=unique(f$tran)
  r.tran=unique(r$tran)

  common=as.character(f.tran[!is.na(match(f.tran, r.tran))])

  f=f[f$tran %in% common,]
  r=r[r$tran %in% common,]

  #empty data frame to populate with matches

  matches=data.frame(yr=integer(),
                     tran=character(),
                     ch=character(),
                     sppn=character(),
                     grp=integer(),
                     unit=character(),
                     front=character(),
                     rear=character(),
                     crew=character(), stringsAsFactors = FALSE
  )


  for (i in 1:length(f$yr)){


    # matches=rbind(matches, c(f$yr[i], f$tran[i], "10", f$sppn[i], f$grp[i], f$unit[i], f$obs[i], r$obs[1], paste(f$obs[i], r$obs[1])))

    for (j in 1:length(r$yr)){

      if(r$matched[j]==1) {next}

      if(r$yr[j]!=f$yr[i]) {next}

      if(r$tran[j]!=f$tran[i]) {next}

      if(r$sppn[j]!=f$sppn[i]) {next}

      if(r$unit[j]!=f$unit[i]) {next}

      if(abs(f$ctime[i]-r$ctime[j])<=time){

        newline=data.frame(yr=f$yr[i],
                           tran=f$tran[i],
                           ch="11",
                           sppn=f$sppn[i],
                           grp=(f$grp[i]+r$grp[j])/2,
                           unit=f$unit[i],
                           front=f$obs[i],
                           rear=r$obs[j],
                           crew=paste(f$obs[i], r$obs[j], sep=""), stringsAsFactors = FALSE
        )

        matches=rbind(matches, newline)



        #matches= rbind(matches, list(f$yr[i], f$tran[i], "11", f$sppn[i], (f$grp[i]+r$grp[j])/2, f$unit[i], f$obs[i], r$obs[j], paste(f$obs[i], r$obs[j])))

        f$matched[i]=1
        r$matched[j]=1
      }

    }

    if (f$matched[i]==0){


      newline=data.frame(yr=f$yr[i],
                         tran=f$tran[i],
                         ch="10",
                         sppn=f$sppn[i],
                         grp=f$grp[i],
                         unit=f$unit[i],
                         front=f$obs[i],
                         rear=r$obs[1],
                         crew=paste(f$obs[i], r$obs[1], sep=""), stringsAsFactors = FALSE
      )

      matches=rbind(matches, newline)
    }

  }


  for (k in 1:length(r$yr)){

    if(r$matched[k]==1) {next}


    newline=data.frame(yr=r$yr[k],
                       tran=r$tran[k],
                       ch="01",
                       sppn=r$sppn[k],
                       grp=r$grp[k],
                       unit=r$unit[k],
                       front=f$obs[1],
                       rear=r$obs[k],
                       crew=paste(f$obs[1], r$obs[k], sep=""), stringsAsFactors = FALSE
    )

    matches=rbind(matches, newline)


    #matches=rbind(matches, c(r$yr[k], r$tran[k], "01", r$sppn[k], r$grp[k], r$unit[k], f$obs[1], r$obs[k], paste(f$obs[1], r$obs[k])))


  }




  return(matches)


}
