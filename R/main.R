

#' Format and summarize a clean data set for analysis
#'
#' DataSelect will combine observation, design transect, and stratification layers into a summary list for analysis
#'
#' DataSelect reads in a clean data file (passed through GreenLight function) and pulls all observations with code==1.  All observations
#' coded as open 1 are then changed to single 1.  The transect layer is then placed on the stratification and trimmed and renumbered from
#' navigational numbers to analysis numbering.  Species codes that need specific project treatments will be changed.  Counts are adjusted
#' for different indices at the row level. The resulting list object is the primary currency for design-based ratio estimator analysis.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param data.path The location of the QA/QC checked data file
#' @param strata.path The location of the analysis stratification .shp file
#' @param transect.path The location of the transect design .shp file
#' @param threshold The distance in kilometers from a design file where observations are no longer counted.  Defaults to 0.5 km.
#'
#' @return List object with 5 elements: \enumerate{
#' \item obs The original data file with corrected transect and stratification information
#' \item flight The flown transects for the observer with corrected numbering
#' \item design The split design information
#' \item strata The spatial polygon summary for the stratification layer
#' \item transect Observations summarized at the transect level
#' }
#'
#' @export
DataSelect <- function(area, data.path=NA, strata.path=NA, transect.path=NA, threshold=.5){


  if(area=="YKDV" || area=="KIG"){area="YKD"}

  data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)

  data=data[data$Code==1,]

  data$Obs_Type[data$Obs_Type=="open" & data$Num==1 & data$Species!="SWANN"]="single"

  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE, area=area)

  data=CorrectTrans(full.data=data, area=area, split.design=split.design, strata.file=strata.path, threshold=threshold)

  flight=TransSummary(full.data=data, split.design=split.design, area=area)
  #flight=flight[flight$len>0,]


  data=SpeciesByProject(full.data=data, area=area)

  if(area=="CRD"){data=TrimToStrata(full.data=data, strata.path=strata.path)}

  data=AdjustCounts(data)

  data=AddClass(data)

  strata=StrataSummary(strata.path)

  data=list("obs"=data, "flight"=flight, "design"=split.design, "strata"=strata)

  if(area=="YKG"){data=FixTavs(selected.data=data)}

  data$obs$ctran=as.character(data$obs$ctran)

  transect.summary=TransData(data)

  data$transect=transect.summary


  return(data)

}



#' Changes species codes based on area
#'
#' SpeciesByProject will look up species codes and potentially alter them based on project area
#'
#' SpeciesByProject is designed to alter species codes by area specifications. Current lists can be found in \code{sppntable}.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data The data frame containing the observations
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#'
#' @return data frame with area-specific species codes
#'
#' @export
SpeciesByProject=function(full.data, area){

  if(area=="YKD" || area=="YKG"){

    for(i in 1:length(full.data$Species)){

      full.data$Species[i]=sppntable$YKD[sppntable$QAQC==full.data$Species[i]][1]

    }

    keepers=unique(sppntable$YKD[sppntable$YKD_EST==1])
    full.data=full.data[full.data$Species %in% keepers,]
  }

  if(area=="ACP"){
    for(i in 1:length(full.data$Species)){

      full.data$Species[i]=sppntable$ACP[sppntable$QAQC==full.data$Species[i]][1]

    }

    keepers=unique(sppntable$ACP[sppntable$ACP_EST==1])
    full.data=full.data[full.data$Species %in% keepers,]}

  if(area=="BLSC"){
    for(i in 1:length(full.data$Species)){

      full.data$Species[i]=sppntable$BLSC[sppntable$QAQC==full.data$Species[i]][1]

    }

    keepers=unique(sppntable$BLSC[sppntable$BLSC_EST==1])
    full.data=full.data[full.data$Species %in% keepers,]
    }

  if(area=="CRD"){
    for(i in 1:length(full.data$Species)){

      full.data$Species[i]=sppntable$CRD[sppntable$QAQC==full.data$Species[i]][1]

    }

    keepers=unique(sppntable$CRD[sppntable$CRD_EST==1])
    full.data=full.data[full.data$Species %in% keepers,]
    }

  if(area=="WBPHS"){
    for(i in 1:length(full.data$Species)){

      full.data$Species[i]=sppntable$WBPHS[sppntable$QAQC==full.data$Species[i]][1]

    }

    keepers=unique(sppntable$WBPHS[sppntable$WBPHS_EST==1])
    full.data=full.data[full.data$Species %in% keepers,]
    }



  return(full.data)

}






#' Aggregate observations by Obs_Type, corrected transect, species, observer, and year
#'
#' TransData will provide an aggregated transect-level summary of all observations in a DataSelect output list by observer and year
#'
#' TransData is designed to take an object from DataSelect and summarize observations at the transect level.  Each row is a unique
#' combination of year, observer, species, transect, and Obs_Type.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param selected.data The data object from DataSelect
#'
#' @return data frame with transect-level summary of observations
#'
#' @export
TransData=function(selected.data){


  agg=aggregate(cbind(Num,itotal,total,ibb, sing1pair2, flock)~Year+Observer+Species+Obs_Type+ctran,data=selected.data$obs, FUN=sum)


  colnames(agg)=c("Year", "Observer", "Species", "Obs_Type", "ctran", "Num", "itotal", "total", "ibb", "sing1pair2", "flock")

  flown.list=unique(selected.data$flight$PartOf)

  for(i in 1:length(flown.list)){

    if( !flown.list[i] %in% unique(agg$ctran)){

      new.row=agg[1,]
      new.row$ctran=flown.list[i]
      new.row$Num=0
      new.row$itotal=0
      new.row$total=0
      new.row$ibb=0
      new.row$sing1pair2=0
      new.row$flock=0

      agg=rbind(agg,new.row)
    }



  }



    agg=as.data.frame(tidyr::complete(data=agg, Year, Observer, Species, Obs_Type, ctran, fill=list(Num=0, itotal=0, total=0, ibb=0, sing1pair2=0, flock=0)))

  agg$area=0

  agg$strata="none"


  selected.data$flight$Strata=as.character(selected.data$flight$Strata)

  for(g in 1:length(agg$area)){

    agg$area[g]=sum(selected.data$flight$SampledArea[selected.data$flight$Year==agg$Year[g] & selected.data$flight$Observer==agg$Observer[g] & selected.data$flight$PartOf==agg$ctran[g]])
    agg$strata[g]=selected.data$flight$Strata[selected.data$flight$Year==agg$Year[g] & selected.data$flight$Observer==agg$Observer[g] & selected.data$flight$PartOf==agg$ctran[g]][1]

  }



return(agg[order(agg$Year, agg$Observer, agg$Species, as.numeric(agg$ctran), agg$Obs_Type),])

}


#' Renumber design transects based on stratification
#'
#' SplitDesign will read transect and stratification layers and number transects based on polygon arrangement in the stratification file
#'
#' SplitDesign is a critically important function to aerial survey analysis.  It will read in design transect and design stratifcation and combine
#' the polygons and lines to renumber the design file appropriately.  It will catch errors such as slight overhang in design transects and strata,
#' incorrect numbering scheme for a given sample, and incorrectly attributed strata to lines.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param strata.file The location of the analysis stratification .shp file
#' @param transect.file The location of the design transect .shp file (currently only supports east-west transects)
#' @param SegCheck Should a map be drawn to verify the process?  TRUE/FALSE
#' @param area If area=="CRD" a specific fix is made to EggIsland north/south transects.  Otherwise area does not matter.
#'
#' @return spatial lines object with appropriate numbering (retains old numbering)
#'
#' @export
SplitDesign <- function(strata.file, transect.file, SegCheck=FALSE, area="other"){


  #read and project transects
  design=rgdal::readOGR(transect.file, layer=tools::file_path_sans_ext(basename(transect.file)), verbose=FALSE)
  design.proj <- sp::spTransform(design, "+proj=longlat +ellps=WGS84 +datum=NAD83")
  design.proj <- smoothr::drop_crumbs(design.proj, threshold=.1)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.file, type="proj")
  strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84 +datum=NAD83")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))

  if("NAME" %in% colnames(strata.proj@data)){
  colnames(strata.proj@data)[colnames(strata.proj@data)=="NAME"]="STRATNAME"
  }

  #intersect transects with strata, create new attribute SPLIT that is a unique
  #numbering system for latitude/strata combos

  newlines = suppressWarnings(raster::intersect(design.proj, strata.proj))
  newlines@data$len=sp::SpatialLinesLengths(newlines, longlat=TRUE)
  newlines=smoothr::drop_crumbs(newlines, threshold=.1)
  newlines@data$id=rownames(newlines@data)
  newlines.proj=sp::spTransform(newlines, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  midpoints=as.data.frame(sp::spTransform(maptools::SpatialLinesMidPoints(newlines.proj), "+proj=longlat +ellps=WGS84 +datum=NAD83"))
  newlines.fort=ggplot2::fortify(newlines, region="STRAT")
  newlines.df=plyr::join(newlines.fort, newlines@data, by="id")
  newlines.df$ROUNDED=round(newlines.df$lat, digits=3)


  if(area=="CRD"){


    for(i in 1:length(newlines.df$STRATNAME)){
    if(newlines.df$STRATNAME[i]=="Egg Island" || newlines.df$STRATNAME[i] == "egg"){newlines.df$ROUNDED[i]= round(newlines.df$long[i], digits=2)}
    }


    }







  newlines.df$SPLIT=as.numeric(factor(interaction(newlines.df$STRATNAME, newlines.df$ROUNDED)))

  newlines@data$SPLIT=0
  newlines@data$mid.Lon=0
  newlines@data$mid.Lat=0

  for (i in 1:length(newlines)){

    newlines@data$SPLIT[i]=newlines.df$SPLIT[newlines.df$OBJECTID==newlines@data$OBJECTID[i] & newlines@data$STRATNAME[i]==newlines.df$STRATNAME][1]
    newlines@data$mid.Lon[i]=midpoints$coords.x1[midpoints$OBJECTID==newlines@data$OBJECTID[i] & newlines@data$STRATNAME[i]==midpoints$STRATNAME][1]
    newlines@data$mid.Lat[i]=midpoints$coords.x2[midpoints$OBJECTID==newlines@data$OBJECTID[i] & newlines@data$STRATNAME[i]==midpoints$STRATNAME][1]

  }

  if(SegCheck==TRUE){

    factpal=leaflet::colorFactor(brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


    map= leaflet() %>%
      addTiles() %>%
      addPolygons(data=strata.proj,
                  fillColor=~factpal(strata.proj$STRATNAME),
                  fillOpacity=.6,
                  stroke=TRUE,
                  color="black",
                  opacity=1,
                  weight=1,
                  popup = paste("Strata: ", strata.proj$STRATNAME)) %>%

      addPolylines(data=newlines,
                   color="black",
                   weight=4,
                   opacity=.9,
                   label=~newlines$OBJECTID,
                   popup = paste("Strata: ", newlines$STRATNAME, "<br>",
                                 "Old Transect: ", newlines$ORIGID, "<br>",
                                 "New Transect: ", newlines$OBJECTID, "<br>",
                                 "Split Transect: ", newlines$SPLIT, "<br>",
                                 "Length: ", newlines$len))  %>%

      addProviderTiles("Esri.WorldImagery")  %>%

      addScaleBar()

    print(map)
    # temp=aggregate(newlines.df$order~newlines.df$OBJECTID+newlines.df$STRAT, FUN="length")
    # colnames(temp)=c("original", "strata", "segs")
    # temp$segs=temp$segs/2
    # temp=temp[order(temp$original, temp$strata),]
    # write.table(temp, file="segcheck.txt", quote=FALSE, row.names=FALSE)


  }


  return(newlines)



}





#' Read .shp file and attach strata names to the object
#'
#' LoadMap will read in a .shp file and return either a data frame or projected spatial object
#'
#' LoadMap is designed to read in a .shp file and attach the strata names to a data frame and return it.  It can also return a projected
#' WGS84 surface.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param map.file The path to the .shp file to be read in
#' @param type Either "df" to return a data frame or "proj" to return a WGS84 projected spatial object
#'
#' @return data frame or projected spatial object
#'
#' @export
LoadMap <- function(map.file, type="df") {



  #maptools::gpclibPermit()
  strata <- rgdal::readOGR(map.file, layer=tools::file_path_sans_ext(basename(map.file)), verbose=FALSE)

  strata.proj <- sp::spTransform(strata, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  #strata<- rgeos::gBuffer(strata, byid=TRUE, width=0)

  if("NAME" %in% colnames(strata.proj@data)){
    colnames(strata.proj@data)[colnames(strata.proj@data)=="NAME"]="STRATNAME"
  }

  if("DESCRIPTIO" %in% colnames(strata.proj@data)){
    colnames(strata.proj@data)[colnames(strata.proj@data)=="DESCRIPTIO"]="STRATNAME"
  }

  strata.proj@data$id = rownames(strata.proj@data)

  #ifelse(area=="acp", strata.fort <- fortify(strata.proj, region="STRATNAME"), strata.fort <- fortify(strata.proj, region="STRAT"))
  strata.fort <- ggplot2::fortify(strata.proj, region="id")

  strata.df=plyr::join(strata.fort, strata.proj@data, by="id")

  if(type=="df") {return(strata.df)}
  if(type=="proj") {return(strata.proj)}
}




#' Display stratification in plot format
#'
#' ViewStrata uses ggplot2 to display a stratification map.
#'
#' This function was replaced with a series of ShowMe functions and is no longer used.  See \code{ShowMe}.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param map The LoadMap object to be displayed
#' @param year The year of the desired transect panel
#' @param ViewTrans Should the transects be plotted?  TRUE/FALSE
#' @param strata Character string of strata to be plotted (or "all")
#' @param numbers Should transects be numbered?  TRUE/FALSE
#' @param print Should the result be plotted (TRUE) or saved (FALSE)
#'
#' @return ggplot2 plot object
#'
#' @export
ViewStrata <- function(map, year=NULL, ViewTrans="none", strata="all", numbers=FALSE, print=TRUE) {
  GIS.obj = LoadMap(map)
if(strata=="all"){

  hulls= GIS.obj %>% group_by(STRATNAME) %>% do(.[chull(.[2:3]),])

  strata.plot <- ggplot(GIS.obj, aes(long, lat, fill=factor(STRATNAME))) +
    geom_polygon(data=hulls, alpha=.3)
    geom_path(data=GIS.obj, aes(long,lat,group=group)  ) +
    geom_path(color="black") +
    coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))




}

  if(strata!="all"){

    data=GIS.obj[as.character(GIS.obj$STRATNAME) %in% strata,]

    strata.plot <- ggplot() +
         geom_path(data=data, aes(long,lat,group=group)  ) +
         geom_path(color="black", lwd=1.5) +
         coord_map(xlim=c(min(data$long), max(data$long)), ylim=c(min(data$lat), max(data$lat))) +
        scale_x_continuous("Longitude (degrees)") + scale_y_continuous("Latitude (degrees)")




  }



  if(ViewTrans != "none"){
    # trans=TranSelect(year=year, area=area)
    # trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
    # trans.layer=trans$layer

    trans.file=ViewTrans

    trans.layer=file_path_sans_ext(basename(trans.file))

    trans.obj=rgdal::readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- sp::spTransform(trans.obj, "+proj=longlat +ellps=WGS84 +datum=NAD83")

    GIS.proj = LoadMap(map, type="proj")

    trans.proj=raster::intersect(trans.proj, GIS.proj)  #trim the excess lines


    trans.proj@data$id = rownames(trans.proj@data)

    trans.df=ggplot2::fortify(trans.proj, region=OBJECTID)

    trans.df=plyr::join(trans.df,trans.proj@data, by="id")

    trans.labels=trans.df[trans.df$order==1,]





    strata.plot = strata.plot +
      geom_path(data=trans.df, aes(x=long, y=lat, group=group))

    if(numbers==TRUE){
    strata.plot = strata.plot +
      geom_text(data=trans.labels, aes(x=long, y=lat, label=ORIGID), size=3, fontface="bold")
    }



  }

  if(print==TRUE){print(strata.plot)}
  return(strata.plot)
}






#' Create index columns based on Num and Obs_Type
#'
#' AdjustCounts will create new columns for 5 indices (itotal, ibb, total, sing1pair2, and flock) and compute the values based on Num and Obs_Type
#'
#' AdjustCounts will take a new observation file and augment the Num category to create what a particular observation means to an index.
#' Currently there are 5 indices that are created: \enumerate{
#' \item itotal - Indicated total.  Singles doubled, pairs doubled, opens added, flkdrake 1-4 doubled, flkdrake 5+ added.
#' \item ibb - Indicated breeding birds.  Singles doubled, pairs doubled, opens removed, flkdrake 1-4 doubled, flkdrake 5+ removed.
#' \item total - Total birds.  Singles added, pairs doubled, opens added, flkdrake added.
#' \item sing1pair2 - Singles and pairs.  Singles added, pairs doubled, opens removed, flkdrake 1-4 added.
#' \item flock - Flocks.  Singles removed, pairs removed, opens added, flkdrake 5+ added.
#' }
#'
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file containing observation history
#'
#' @return data frame of original data and 5 new index columns
#'
#' @export
AdjustCounts <- function(full.data){

    full.data$itotal=0
    full.data$ibb=0
    full.data$total=0
    full.data$sing1pair2=0
    full.data$flock=0

    for(i in 1:length(full.data$Num)){

    if(is.na(full.data$Obs_Type[i])){next}

    #Double singles for indicated totals
    if(full.data$Obs_Type[i]=="single") {
      full.data$itotal[i]=2*full.data$Num[i]
      full.data$ibb[i]=2*full.data$Num[i]
      full.data$total[i]=full.data$Num[i]
      full.data$sing1pair2[i]=full.data$Num[i]
      full.data$flock[i]=0
    }

    #Pairs are doubled for both total and indicated total
    else if(full.data$Obs_Type[i]=="pair") {
      full.data$itotal[i]=2*full.data$Num[i]
      full.data$ibb[i]=2*full.data$Num[i]
      full.data$total[i]=2*full.data$Num[i]
      full.data$sing1pair2[i]=2*full.data$Num[i]
      full.data$flock[i]=0

    }

    #Open indicates a flock, nothing doubled, zero for ibb
    else if(full.data$Obs_Type[i]=="open"){
      full.data$itotal[i]=full.data$Num[i]
      full.data$total[i]=full.data$Num[i]
      full.data$ibb[i]=0
      full.data$sing1pair2[i]=0
      full.data$flock[i]=full.data$Num[i]


    }

    #Flocked drakes are doubled for 1-4 seen for indicated bb/totals.  Reference would be useful.
    #Added 1-4 flkdrake to sing1pair2 in 2024, removed them from flock index.

    else if(full.data$Obs_Type[i]=="flkdrake" & full.data$Num[i]<5){
      full.data$itotal[i]=2*full.data$Num[i]
      full.data$total[i]=full.data$Num[i]
      full.data$ibb[i]=2*full.data$Num[i]
      full.data$sing1pair2[i]=full.data$Num[i]
      full.data$flock[i]=0

    }

    #Flocked drakes 5 and above aren't doubled because of science stuff, 0 for ibb.
    else if(full.data$Obs_Type[i]=="flkdrake" & full.data$Num[i]>4){
      full.data$itotal[i]=full.data$Num[i]
      full.data$total[i]=full.data$Num[i]
      full.data$ibb[i]=0
      full.data$sing1pair2[i]=0
      full.data$flock[i]=full.data$Num[i]


    }

}

  return(full.data)
}



#' Summarize indices by year, observer, transect, species, and strata
#'
#' CountsTable will summarize an AdjustCounts data frame into a table
#'
#' CountsTable is designed to be run on an adjusted data file (has been run through AdjustCounts to create indices).  It will return a tabular summary of counts.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param adj.counts A data frame that has been through AdjustCounts
#'
#' @return data frame with tabular summary of observations
#'
#' @export
CountsTable=function(adj.counts) {



    t1=(aggregate(adj.counts$total~adj.counts$Year+adj.counts$Observer+adj.counts$ctran+adj.counts$Species+adj.counts$strata, FUN=sum))
    t2=(aggregate(adj.counts$itotal~adj.counts$Year+adj.counts$Observer+adj.counts$ctran+adj.counts$Species+adj.counts$strata, FUN=sum))
    t2b=aggregate(adj.counts$ibb~adj.counts$Year+adj.counts$Observer+adj.counts$ctran+adj.counts$Species+adj.counts$strata, FUN=sum)
    t4=aggregate(adj.counts$sing1pair2~adj.counts$Year+adj.counts$Observer+adj.counts$ctran+adj.counts$Species+adj.counts$strata, FUN=sum)
    t5=aggregate(adj.counts$flock~adj.counts$Year+adj.counts$Observer+adj.counts$ctran+adj.counts$Species+adj.counts$strata, FUN=sum)

    t3=merge(t1,t2,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$ctran", "adj.counts$Species", "adj.counts$strata"))
    t3=merge(t3,t2b,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$ctran", "adj.counts$Species", "adj.counts$strata"))
    t3=merge(t3,t4,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$ctran", "adj.counts$Species", "adj.counts$strata"))
    t3=merge(t3,t5,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$ctran", "adj.counts$Species", "adj.counts$strata"))


    colnames(t3)=c("Year","Observer","ctran", "Species", "strata", "total", "itotal", "ibb", "sing1pair2", "flock")

    return(t3[order(t3$Year, t3$Observer, t3$Species, as.numeric(t3$ctran)),])




} #end CountsTable





#' Sample the polygon under an observation
#'
#' PointsToStrata will attribute each observation in a file to underlying strata
#'
#' PointsToStrata will sample the polygon layer under each observation and replace the Strata column entry with the appropriate strata.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A data frame of observations.
#' @param area The project area
#'
#' @return data frame with Strata column overwritten
#'
#' @export
PointsToStrata=function(full.data, area){


  #full.data=SpatialNA(full.data)

  x=na.approx(full.data$long, na.rm=FALSE)
  y=na.approx(full.data$lat, na.rm=FALSE)


  sp=cbind(x,y)

  sp=sp::SpatialPoints(sp)

  sp::proj4string(sp)=sp::CRS("+proj=longlat +ellps=WGS84 +datum=NAD83")

  sp=sp::spTransform(sp, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  map=LoadMap(area, type="proj")

  map=sp::spTransform(map, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


  full.data$strat=over(sp,map)$STRAT



  if(area != "crd"){

  for(a in 1:length(full.data$strat)){
    #print(a)
    #If NA found, replace with strata type shared by entries on that transect
    #This assumes the click/logging was actually over the strata and not ACTUALLY early/late
    if(is.na(full.data$strat[a])){

      temp=full.data[full.data$tran==full.data$tran[a],]


      #full.data$strat[a]=names(sort(table(temp$strat), decreasing=TRUE))[1]
      if(length(which(!is.na(temp$strat)))>0){
      full.data$strat[a]=names(which.max(table(temp$strat)))
      }
      else{full.data$strat[a]="undefined"}

    }

  }
}



  return(full.data)

}


# Commented out 4/2020
#
# TranSelect = function(year, area){
#
#   if(area=="ykg"){
#
#   trans=list("file"="YKG_2018_MemoTrans.shp", "layer"="YKG_2018_MemoTrans")
#   }
#
#   if(area=="crd"){
#
#     trans=list("file"="CRD_2018_Transects.shp", "layer"="CRD_2018_Transects")
#     return(trans)
#     }
#
#
#   if(area=="acp"){
#
#     trans=list("file"="ACP_2018_Transects.shp", "layer"="ACP_2018_Transects")
#   }
#
#
#
#   return(trans)
# }
#
#




#' Standard ratio estimator for aerial survey data
#'
#' Densities will combine spatially-referenced observations with design transects and strata to create an index estimate
#'
#' Densities is the primary function in AKaerial for producing index estimates. It will take an object from DataSelect and adjust counts, summarize
#' spatial information, and calculate indices and their associated standard errors.  Also retained are estimates of densities of bird by strata and on
#'  each transect.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The DataSelect list object to be analyzed
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param output TRUE/FALSE Should results be written to text files in the current working directory?
#'
#' @return List object with 2 elements: \enumerate{
#' \item estimates Observer-specific estimates for the species indicated in the sppntable estimates column
#' \item counts.final Deeper level count information by transect, strata, species, and observer
#' }
#'
#' @export
Densities=function(data, area, output=FALSE) {

  #Save old warning settings to revert to before leaving function
  oldw <- getOption("warn")
  #Suppress spatial warnings inside function call
  options(warn=-1)


  if(area=="YKDV"){area="YKD"}


  #Sum the counts by combinations of species/transect
  counts.t=CountsTable(data$transect)
  counts.t$SampledArea=0

  for (i in 1:length(counts.t$SampledArea)){

    counts.t$SampledArea[i]=sum(data$flight$SampledArea[data$flight$PartOf==counts.t$ctran[i]])
    #counts.t$area[i]=data$flight$SampledArea[data$flight$Year==counts.t$Year[i] & data$==counts.t$obs[i] & data$ctran==counts.t$ctran[i]][1]

  }

  #Add a row with a 0 count for every species counted somewhere in the data but not on a given transect
  #t3=MakeZeroes(data, counts.t)
   t3=counts.t


  #Sum the counts of each species by strata type
  sp.strat.total=aggregate(t3$total~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=sum)
  colnames(sp.strat.total)=c("Year","Observer", "Species", "strata", "total")

  sp.strat.itotal=aggregate(t3$itotal~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=sum)
  colnames(sp.strat.itotal)=c("Year","Observer", "Species", "strata", "itotal")

  sp.strat.ibb=aggregate(t3$ibb~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=sum)
  colnames(sp.strat.ibb)=c("Year","Observer", "Species", "strata", "ibb")

  sp.strat.sing1pair2=aggregate(t3$sing1pair2~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=sum)
  colnames(sp.strat.sing1pair2)=c("Year","Observer", "Species", "strata", "sing1pair2")

  sp.strat.flock=aggregate(t3$flock~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=sum)
  colnames(sp.strat.flock)=c("Year","Observer", "Species", "strata", "flock")


  #Variance of the counts within each strata
  sp.strat.total.v=aggregate(t3$total~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=var)
  colnames(sp.strat.total.v)=c("Year", "Observer", "Species", "strata", "total.v")

  sp.strat.itotal.v=aggregate(t3$itotal~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=var)
  colnames(sp.strat.itotal.v)=c("Year", "Observer", "Species", "strata", "itotal.v")

  sp.strat.ibb.v=aggregate(t3$ibb~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=var)
  colnames(sp.strat.ibb.v)=c("Year","Observer", "Species", "strata", "ibb.v")

  sp.strat.sing1pair2.v=aggregate(t3$sing1pair2~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=var)
  colnames(sp.strat.sing1pair2.v)=c("Year","Observer", "Species", "strata", "sing1pair2.v")

  sp.strat.flock.v=aggregate(t3$flock~t3$Year+t3$Observer+t3$Species+t3$strata, FUN=var)
  colnames(sp.strat.flock.v)=c("Year","Observer", "Species", "strata", "flock.v")

  sp.strat=merge(sp.strat.total, sp.strat.itotal)
  sp.strat=merge(sp.strat, sp.strat.ibb)
  sp.strat=merge(sp.strat, sp.strat.sing1pair2)
  sp.strat=merge(sp.strat, sp.strat.flock)

  sp.strat.v=merge(sp.strat.total.v, sp.strat.itotal.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.ibb.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.sing1pair2.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.flock.v)


  #Put the totals together and leave placeholders for var and cov
  sp.strat.final=merge(sp.strat, sp.strat.v)
  sp.strat.final$total.cov=0
  sp.strat.final$itotal.cov=0
  sp.strat.final$var.N=0
  sp.strat.final$var.Ni=0
  sp.strat.final$ibb.cov=0
  sp.strat.final$var.Nib=0
  sp.strat.final$sing1pair2.cov=0
  sp.strat.final$var.Nsing1pair2=0
  sp.strat.final$flock.cov=0
  sp.strat.final$var.Nflock=0

  #Calculate covariance of total counts and area sampled

  for (i in 1:length(sp.strat.final$strata)){

    temp.t3=t3[t3$Year==sp.strat.final$Year[i] & t3$Observer==sp.strat.final$Observer[i] & t3$Species==sp.strat.final$Species[i] & t3$strata==sp.strat.final$strata[i],]
    sp.strat.final$total.cov[i]=cov(temp.t3$total, temp.t3$SampledArea)
    sp.strat.final$itotal.cov[i]=cov(temp.t3$itotal, temp.t3$SampledArea)
    sp.strat.final$ibb.cov[i]=cov(temp.t3$ibb, temp.t3$SampledArea)
    sp.strat.final$sing1pair2.cov[i]=cov(temp.t3$sing1pair2, temp.t3$SampledArea)
    sp.strat.final$flock.cov[i]=cov(temp.t3$flock, temp.t3$SampledArea)

  }


  #Calculate the total area by type and the variance of the areas

  area.strat=aggregate(t3$SampledArea~t3$Year+t3$Observer+t3$strata+t3$Species, FUN=sum)
  area.strat.v=aggregate(t3$SampledArea~t3$Year+t3$Observer+t3$strata+t3$Species, FUN=var)

  colnames(area.strat)=c("Year", "Observer", "strata", "Species","total.area")
  colnames(area.strat.v)=c("Year", "Observer", "strata", "Species", "total.area.var")

  area.strat=area.strat[!duplicated(area.strat[1:3]),-4]
  area.strat.v=area.strat.v[!duplicated(area.strat.v[1:3]),-4]


  #Put spatial summary together
  area.summary=merge(area.strat, area.strat.v)
  #print(area.summary)

  #Merge the counts and spatial stats
  counts.final=merge(sp.strat.final,area.summary, by=c("Year", "Observer", "strata"))

  #Calculate final densities for each strata layer
  density.total=counts.final$total/counts.final$total.area
  density.itotal=counts.final$itotal/counts.final$total.area
  density.ibb=counts.final$ibb/counts.final$total.area
  density.sing1pair2=counts.final$sing1pair2/counts.final$total.area
  density.flock=counts.final$flock/counts.final$total.area


  counts.final=cbind(counts.final, density.total, density.itotal, density.ibb, density.sing1pair2, density.flock)
  #print(head(counts.final))

  #Get actual areas from gis layers
  #strata.area=aggregate(shp@data$AREA~shp@data$STRAT, FUN=sum)
  #colnames(strata.area)=c("strata", "layer.area")

  #Convert from m^2 to km^2
  #strata.area$layer.area=strata.area$layer.area / 1000000

  #print(strata.area)

  counts.final=merge(counts.final, data$strata, by="strata")

  #Extrapolate density estimates across area calculation
  total.est=counts.final$density.total * counts.final$layer.area
  itotal.est=counts.final$density.itotal * counts.final$layer.area
  ibbtotal.est=counts.final$density.ibb * counts.final$layer.area
  sing1pair2.est=counts.final$density.sing1pair2 * counts.final$layer.area
  flock.est=counts.final$density.flock * counts.final$layer.area


  counts.final=cbind(counts.final, total.est, itotal.est,ibbtotal.est, sing1pair2.est, flock.est)

  counts.final=counts.final[counts.final$strata != "Nonhabitat", ]
  counts.final=counts.final[counts.final$strata != "Mountains", ]
  counts.final=counts.final[counts.final$strata != "zero", ]



  #Summarize in table
  estimates=aggregate(counts.final$total.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates)=c("Year", "Observer", "Species", "total.est")

  estimates.i=aggregate(counts.final$itotal.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.i)=c("Year", "Observer", "Species","itotal.est")

  estimates.ibb=aggregate(counts.final$ibbtotal.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.ibb)=c("Year", "Observer", "Species","ibbtotal.est")

  estimates.sing1pair2=aggregate(counts.final$sing1pair2.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.sing1pair2)=c("Year", "Observer", "Species","sing1pair2.est")

  estimates.flock=aggregate(counts.final$flock.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.flock)=c("Year", "Observer", "Species","flock.est")

  estimates=merge(estimates, estimates.i, by=c("Year", "Observer", "Species"))
  estimates=merge(estimates, estimates.ibb, by=c("Year", "Observer", "Species"))
  estimates=merge(estimates, estimates.sing1pair2, by=c("Year", "Observer", "Species"))
  estimates=merge(estimates, estimates.flock, by=c("Year", "Observer", "Species"))


  #adj.counts=merge(adj.counts, transects, by.x="tran", by.y="original")

  ### Var(N) ###


  reps2=aggregate(t3$ctran~t3$Year+t3$Observer+t3$strata+t3$Species, FUN=length)
  colnames(reps2)=c("Year", "Observer", "strata", "Species","m")

  reps2=reps2[!duplicated(reps2[1:3]),-4]


  data$strata=merge(data$strata, reps2, by="strata")

  #diff.lat=merge(diff.lat, area.summary, by=c("yr", "obs", "strata"))

  #replace the Egg Island N/S facing transect M calculation
  #Note-moved this to StrataSummary
  #if(area=="crd"){diff.lat$M[diff.lat$strata=="Egg Island"]=10/(trans.width*n.obs)}

  #print(diff.lat)

  #print(diff.lat)
  #See equation 12.9, p. 249 in "Analysis and Management of Animal Populations"
  #Williams, Nichols, Conroy; 2002

  counts.final$m=0

  for (j in 1:length(counts.final$Species)){

    M=data$strata$M[data$strata$Year==counts.final$Year[j] & data$strata$Observer==counts.final$Observer[j] & data$strata$strata==counts.final$strata[j]]
    m=data$strata$m[data$strata$Year==counts.final$Year[j] & data$strata$Observer==counts.final$Observer[j] & data$strata$strata==counts.final$strata[j]]
    prop.m=((1-(m/M))/m)

    counts.final$m[j]=m

    #if(counts.final$sppn[j]=="SPEI"){print((counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j])))}

    counts.final$var.N[j]=(M^2)*prop.m*(counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j]))
    counts.final$var.Ni[j]=(M^2)*prop.m*(counts.final$itotal.v[j]+(counts.final$density.itotal[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.itotal[j]*counts.final$itotal.cov[j]))
    counts.final$var.Nib[j]=(M^2)*prop.m*(counts.final$ibb.v[j]+(counts.final$density.ibb[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.ibb[j]*counts.final$ibb.cov[j]))
    counts.final$var.Nsing1pair2[j]=(M^2)*prop.m*(counts.final$sing1pair2.v[j]+(counts.final$density.sing1pair2[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.sing1pair2[j]*counts.final$sing1pair2.cov[j]))
    counts.final$var.Nflock[j]=(M^2)*prop.m*(counts.final$flock.v[j]+(counts.final$density.flock[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.flock[j]*counts.final$flock.cov[j]))


  }


  var.est=aggregate(counts.final$var.N~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est)=c("Year", "Observer", "Species","var.N")

  var.est.i=aggregate(counts.final$var.Ni~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.i)=c("Year", "Observer", "Species","var.Ni")

  var.est.ibb=aggregate(counts.final$var.Nib~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.ibb)=c("Year", "Observer", "Species","var.Nib")

  var.est.sing1pair2=aggregate(counts.final$var.Nsing1pair2~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.sing1pair2)=c("Year", "Observer", "Species","var.Nsing1pair2")

  var.est.flock=aggregate(counts.final$var.Nflock~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.flock)=c("Year", "Observer", "Species","var.Nflock")

  estimates=merge(estimates, var.est, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.i, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.ibb, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.sing1pair2, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.flock, by=c("Year", "Observer", "Species"), all=TRUE)


  estimates$SE=sqrt(estimates$var.N)
  estimates$SE.i=sqrt(estimates$var.Ni)
  estimates$SE.ibb=sqrt(estimates$var.Nib)
  estimates$SE.sing1pair2=sqrt(estimates$var.Nsing1pair2)
  estimates$SE.flock=sqrt(estimates$var.Nflock)

#
#   estimates$total.est=as.integer(estimates$total.est)
#   estimates$itotal.est=as.integer(estimates$itotal.est)
#   estimates$ibbtotal.est=as.integer(estimates$ibbtotal.est)
#   estimates$sing1pair2.est=as.integer(estimates$sing1pair2.est)
#   estimates$flock.est=as.integer(estimates$flock.est)
#

  options(warn = oldw)

  #Output tables to txt files if requested

  if(output==TRUE){
    write.table(counts.final, file="finalcounts.txt", quote=FALSE, row.names=FALSE)
    write.table(estimates, file="estimates.txt", quote=FALSE, row.names=FALSE)

  }

  return(list("estimates"=estimates, "counts.final"=counts.final))
}

#' Combine estimates and variances for an observer crew
#'
#' CombineEstimates will take observer-specific estimates and summarize them as a crew estimate for a given year
#'
#' CombineEstimates will take observer-specific estimates and summarize them as a crew estimate for a given year
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param estimates The \code{estimates} element of the list returned in Densities
#'
#' @return data frame of pooled estimates
#'
#' @export
CombineEstimates=function(estimates){

  yr.list=unique(estimates$Year)
  sp.list=unique(estimates$Species)

  combined=data.frame(Year=rep(yr.list, each=length(unique(estimates$Species))), Species=rep(sp.list, length(yr.list)), total=0, total.var=0, total.se=0, itotal=0, itotal.var=0, itotal.se=0, ibb=0, ibb.var=0, ibb.se=0, sing1pair2=0, sing1pair2.var=0, sing1pair2.se=0, flock=0, flock.var=0, flock.se=0)

  for(i in 1:length(combined$Year)){


      combined$total[i]=mean(estimates$total.est[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

      combined$itotal[i]=mean(estimates$itotal.est[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

      combined$ibb[i]=mean(estimates$ibbtotal.est[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

      combined$sing1pair2[i]=mean(estimates$sing1pair2.est[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

      combined$flock[i]=mean(estimates$flock.est[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

       combined$total.var[i]=sum(estimates$var.N[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.N[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

      combined$itotal.var[i]=sum(estimates$var.Ni[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Ni[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

      combined$ibb.var[i]=sum(estimates$var.Nib[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nib[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

      combined$sing1pair2.var[i]=sum(estimates$var.Nsing1pair2[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nsing1pair2[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

      combined$flock.var[i]=sum(estimates$var.Nflock[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nflock[estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

    }

  combined$total.se=sqrt(combined$total.var)
  combined$itotal.se=sqrt(combined$itotal.var)
  combined$ibb.se=sqrt(combined$ibb.var)
  combined$sing1pair2.se=sqrt(combined$sing1pair2.var)
  combined$flock.se=sqrt(combined$flock.var)

  combined[is.na(combined)]=0

  return(combined)

}



# Commented out 4/2020
#
# MakeZeroes=function(full.data){
#
#   #Appends a count of 0 to transects that were flown but did not record any of a given species
#
#   #list of years
#   year.list=as.numeric(unique(full.data$yr))
#   #list of observers
#   obs.list=as.character(unique(full.data$obs))
#   #list of transects
#   tran.list=as.character(unique(full.data$ctran))
#   #list of species
#   sp.list=as.character(unique(full.data$sppn))
#
#
#   #cycle through, check each transect/observer combo for each species
#   for (h in 1:length(year.list)){
#     print(paste("Making zeroes for ", year.list[h]))
#   for (i in 1:length(sp.list)){
#     for (j in 1:length(tran.list)){
#       for (k in 1:length(obs.list)){
#
#   #skip if count exists
#       if(any(as.character(full.data$sppn)==sp.list[i] & as.character(full.data$ctran)==tran.list[j] & as.character(full.data$obs)==obs.list[k] & full.data$yr==year.list[h]))
#       {next}
#
#   #make sure that transect was flown
#       if(any(as.character(full.data$obs)==obs.list[k] & as.character(full.data$ctran)==tran.list[j] & full.data$yr==year.list[h]))
#       {
#   #add the 0 row
#         new.row=c(year.list[h], NA, NA, NA, obs.list[k], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list[i], 0, "open", NA, NA, tran.list[j], NA, 0)
#       full.data=rbind(full.data,new.row)
#       }
#       } #end obs
#
#   } #end tran
#   } #end sp
#   } #end year
#
#   return(full.data)
#
# }
#




# Commented out 4/2020
#
# TransectTable <- function(trans.file, trans.layer, obs=1, method, area) {
#
#
#   if(method=="gis" & area != "acp"){
#
#     #split the design shp file into component segments (file has each transect cutting through
#     #multiple strata and keeping same number)
#
#     trans.points=SplitDesign(file.name=trans.file, layer.name=trans.layer, area=area)
#
#     id=trans.points$id
#
#     x=trans.points$long
#     y=trans.points$lat
#
#
#
#     }
#
#   if(method=="gis" & area == "acp"){
#
#
#     trans.obj=rgdal::readOGR(trans.file, trans.layer, verbose=FALSE)
#     trans.proj <- sp::spTransform(trans.obj, "+proj=longlat +ellps=WGS84")
#
#     strata.proj=LoadMap(area, type="proj")
#
#     newlines = raster::intersect(trans.proj, strata.proj)
#     newlines@data$id=rownames(newlines@data)
#     newlines.fort=ggplot2::fortify(newlines, region="STRAT")
#     trans.points=plyr::join(newlines.fort, newlines@data, by="id")
#
#     id=trans.points$id
#     x=vector("numeric",length=2*length(unique(trans.points$id)))
#     y=vector("numeric",length=2*length(unique(trans.points$id)))
#     id=unique(trans.points$id)
#
#     original=vector("character",length=length(unique(trans.points$id)))
#     renum=vector("character",length=length(unique(trans.points$id)))
#     type=vector("character",length=length(unique(trans.points$id)))
#
#     for(i in 1:length(id)){
#       x[2*i-1]=trans.points$long[trans.points$id==id[i] & trans.points$order==1]
#       x[2*i]=trans.points$long[trans.points$order==which.max(trans.points$order[trans.points$id==id[i]]) & trans.points$id==id[i]]
#       y[2*i-1]=trans.points$lat[trans.points$id==id[i] & trans.points$order==1]
#       y[2*i]=trans.points$lat[trans.points$order==which.max(trans.points$order[trans.points$id==id[i]]) & trans.points$id==id[i]]
#      original[i]=trans.points$ORIGID[trans.points$id==id[i] & trans.points$order==1]
#      renum[i]=as.character(trans.points$OBJECTID[trans.points$id==id[i] & trans.points$order==1])
#      type[i]=trans.points$STRAT[trans.points$id==id[i] & trans.points$order==1]
#
#
#      }
#
#
#
#
#   }
#
#
#
#
#
#   #pulls in design file that is a list of start and end points
#
#   if(method=="text"){
#   trans.points=trans.file
#   code=as.character(trans.points$ident)
#   id=code
#   side=code
#   original=NULL
#
#   for (i in 1:length(unique(code))){
#
#       id[i]=as.numeric(substr(code[i],nchar(code[i])-2, nchar(code[i])-1))
#       side[i]=substr(code[i], nchar(code[i]), nchar(code[i]))
#
#
#   }
#   }
#
#   #either method above results in x,y and the following code calculates great circle distance
#   #for each set of coordinates
#
#
#   width=obs*.2
#
#
#   id=as.numeric(id)
#
#   d=vector("numeric",length=length(unique(id)))
#
#   #type=vector("character", length=length(unique(id)))
#
#   sp=cbind(x,y)
#   sp.which=seq(1,length(x), by=2)
#   sp.set=data.frame(x=((sp[sp.which,1]+sp[sp.which+1,1])/2), y=sp[sp.which,2])
#
#
#   sp.set=SpatialPoints(sp.set)
#   sp::proj4string(sp.set)=sp::CRS("+proj=longlat +ellps=WGS84")
#
#
#   if(method=="text"){type=over(sp.set,LoadMap(area, type="proj"))$STRATNAME}
#   #if(method=="gis"){type=trans.points$STRAT[seq(1,length(trans.points$STRAT), by=2)]}
#
#   #gcd.slc works best for a matrix of coords
#   for (i in 1:length(d)){
#
#   coords=matrix(c(x[2*i-1], x[2*i], y[2*i-1], y[2*i]), nrow=2, ncol=2)
#
#
#   d[i]=gcd.slc(coords[1,1], coords[1,2], coords[2,1], coords[2,2])
#
#
#   }
#
#
#   output=data.frame(oldid=original, newid=renum, d=d, type=type, des.area=width*d)
#   output=output[output$d>0.25,]
#
#
#
#   if(method=="text"){output$original=output$id}
#
#
#   #output=output[output$d>0.5,]
#   print(output)
#   return(output)
#
#   }


#' Convert degrees to radians
#'
#' Convert degrees to radians.
#'
#' Convert degrees to radians
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param deg Input number in degrees
#'
#' @return Radian conversion
#'
#' @export
deg2rad <- function(deg) return(deg*pi/180)


#' Spherical distance between 2 points
#'
#' Calculates the geodesic distance between two points specified by radian latitude/longitude using the Spherical Law of Cosines (slc)
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param long1 Longitude of point 1 in degrees
#' @param lat1 Latitude of point 1 in degrees
#' @param long2 Longitude of point 2 in degrees
#' @param lat2 Latitude of point 2 in degrees
#'
#' @return distance in km
#'
#' @export
gcd.slc <- function(long1, lat1, long2, lat2) {
  long1=deg2rad(long1)
  lat1=deg2rad(lat1)
  long2=deg2rad(long2)
  lat2=deg2rad(lat2)

  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}



#' Find gaps between polygons of the same stratum type
#'
#' FindVoids will find the gaps between discontinuous polygons and remove them from potential available transect calculations
#'
#' Findvoids is used in the maximum available transect (M) calculation in the variance estimate in the ratio estimator. It removes gaps
#' in strata from the maximum-minimum latitude calculation.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param pieces Polygons within a given stratum
#'
#' @return data frame of voids and lengths
#'
#' @export
FindVoids= function(pieces) {

  voids=data.frame("id"=0,"d"=0)



  for (i in 1:(length(pieces$id)-1)){
#check sequential pieces (sorted by decreasing maximum northernmost values) for voids
     if(pieces$id[i]==pieces$id[i+1]){
       temp=data.frame("id"=pieces$id[i], "d"=pieces$min[i]-pieces$max[i+1])
       voids=rbind(voids,temp)
     }



  }

  #any positive values indicate actual voids in sampled area
  voids=voids[voids$d>0, ]
  if(sum(voids$d>0)){
  voids=aggregate(voids$d~voids$id, FUN=sum)
  colnames(voids)=c("id", "d")
  }
  return(voids)

}

#
# TransectLevel=function(data, n.obs=2, trans.method="gis", trans.width=.2, area) {
#
#   trans=TranSelect(year=data$yr[1], area=area)
#
#   trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
#   trans.layer=trans$layer
#   shp=LoadMap(area=area,type="proj")
#
#   #Save old warning settings to revert to before leaving function
#   oldw <- getOption("warn")
#   #Suppress spatial warnings inside function call
#   options(warn=-1)
#
#
#   data$strat=as.character(data$strat)
#
#   #Compile the length (d), strata (type), area covered (des.area), and original trans name in data (original)
#   #Splits transects by strata if needed
#   transects=TransectTable(trans.file=trans.file, trans.layer=trans.layer, method=trans.method, area=area, obs=n.obs)
#   #transects=transects[,-1]
#
#
#   #Compute the total/indicated total for the group sizes indicated in the data
#   adj.counts=AdjustCounts(data)
#
#   #Sum the counts by combinations of species/transect
#   counts.t=CountsTable(adj.counts)
#
#   return(counts.t)
# }
#



#' Replace navigational transect (tran) with corrected transect (ctran)
#'
#' CorrectTrans will cross-reference original navigational numbering of transects with corrected SplitDesign numbering
#'
#' CorrectTrans uses SplitDesign output to overwrite potentially incorrect navigational transect numbering with sample-appropriate numbering.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file of observations
#' @param threshold The distance in kilometers from a design file where observations are no longer counted.  Defaults to 0.5 km.
#' @param split.design SplitDesign output object of corrected transect lines
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param strata.file The path to the .shp file of the stratification
#'
#' @return original data frame (full.data) with ctran column added for corrected transect number
#'
#' @export
CorrectTrans=function(full.data, threshold=.5, split.design, area, strata.file){

  split.design=sp::spTransform(split.design, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

for (t in 1:length(full.data$Lon)){

  if(is.na(full.data$Lon[t])){

    full.data$Lon[t]=split.design$mid.Lon[split.design$ORIGID==full.data$Transect[t]][1]

  }

  if(is.na(full.data$Lat[t])){

    full.data$Lat[t]=split.design$mid.Lat[split.design$ORIGID==full.data$Transect[t]][1]

  }

  if(full.data$Lon[t]==0){

    full.data$Lon[t]=split.design$mid.Lon[split.design$ORIGID==full.data$Transect[t]][1]

  }

  if(full.data$Lat[t]==0){

    full.data$Lat[t]=split.design$mid.Lat[split.design$ORIGID==full.data$Transect[t]][1]

  }


}


sp::coordinates(full.data)=~Lon+Lat

sp::proj4string(full.data)=sp::CRS("+proj=longlat +ellps=WGS84 +datum=NAD83")


# if(area=="CRD"){
# strata.proj=LoadMap(strata.file, type="proj")
# strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84")
#
# full.data=raster::intersect(full.data, strata.proj)
# }

full.data=sp::spTransform(full.data, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")



full.data$ctran=full.data$Transect
full.data$closest=full.data$Transect
full.data$dist=0


for (j in seq_along(full.data$closest)){
  full.data$closest[j]=as.numeric(as.character(split.design$SPLIT[which.min(suppressWarnings(rgeos::gDistance(full.data[j,],split.design,byid=TRUE)))]))
  full.data$dist[j]=min(suppressWarnings(rgeos::gDistance(full.data[j,],split.design,byid=TRUE)))
}


# if(area=="CRD"){
#
#
#   for (k in 1:length(full.data$ctran)){
#   full.data$ctran[k]=split.design$SPLIT[split.design$ORIGID==full.data$Transect[k]][1]
#
#   }
#
#   full.data$dist = full.data$dist/1000
#   full.data=sp::spTransform(full.data, "+proj=longlat +ellps=WGS84 +datum=NAD83")
#
#
# }


# if(area != "CRD"){

  full.data$ctran=full.data$closest

  #convert from meters to km, then re-project to lat/lon
  full.data$dist = full.data$dist/1000
  full.data=sp::spTransform(full.data, "+proj=longlat +ellps=WGS84 +datum=NAD83")

  full.data$ctran[full.data$dist>threshold]=NA

#}


full.data=full.data[!(is.na(full.data$ctran)), ]

ORIG.list=as.data.frame(split.design[,c("ORIGID", "SPLIT")])
flown.ORIG=unique(full.data$Transect)
flown.SPLIT=unique(full.data$SPLIT)

for(i in 1:length(ORIG.list$SPLIT)){


  #suppressWarnings(if(any(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]])[!(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]]) %in% unique(full.data$ctran[full.data$Transect==ORIG.list[i]]))])){

  if((ORIG.list$ORIGID[i] %in% flown.ORIG) & !(ORIG.list$SPLIT[i] %in% flown.SPLIT)){

    #missing=unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]])[!(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]]) %in% unique(full.data$ctran[full.data$Transect==ORIG.list[i]]))]

    missing=ORIG.list$SPLIT[i]

      dummy.row=full.data[1,]

      dummy.row$ctran=missing
      dummy.row$Transect=ORIG.list$ORIGID[i]
      dummy.row$Species="START"
      dummy.row$Num=0
      #dummy.row$Lat=split.design$mid.Lat[split.design$ORIGID==ORIG.list[i]]
      #dummy.row$Lon=split.design$mid.Lon[split.design$ORIGID==ORIG.list[i]]

      full.data=rbind(full.data, dummy.row)
    }




}

full.data$ctran=as.character(full.data$ctran)

return(as.data.frame(full.data))

}  #end CorrectTrans()


#
# PlotObs=function(strata.plot, selected.data, multiyear=TRUE, labelyear=FALSE, box=FALSE, set.box=c(-9999,0,0,0)){
#
#   if(multiyear==TRUE){
#
#     strata.plot= strata.plot + geom_point(data=selected.data, aes(x=long, y=lat))
#
#
#   }
#
#   if(labelyear==TRUE){
#
#     strata.plot= strata.plot + geom_text(data=selected.data, aes(x=long, y=lat, label=yr), hjust=0, vjust=0)
#
#
#   }
#
# if (box==TRUE){
#   sp::coordinates(selected.data)=~long+lat
#   bound=bbox(selected.data)
#
#   strata.plot= strata.plot + coord_map(xlim=c(bound[1,1]-.5, bound[1,2]+.5), ylim=c(bound[2,1]-.25, bound[2,2]+.25))
#
# }
#
# if (set.box[1]!=-9999){
#
#   strata.plot= strata.plot + coord_map(xlim=c(set.box[1], set.box[2]), ylim=c(set.box[3], set.box[4]))
#
#   #Barrow set.box=c(-157.5,-155,70.75,71.4)
#
# }
#
#
# print(strata.plot)
# return(strata.plot)
# }
#
#



#' Summarize the flight information for an observer
#'
#' TransSummary will summarize the flight and sampled area of a pilot or observer and give both the original and renumbered transect for reference
#'
#' TransSummary uses what was declared by a pilot or observed as navigational transect number in their transcribed data file and cross references it
#' with the corrected transect number (ctran) in split design to develop a flight record for the pilot/observer.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file of observations
#' @param split.design SplitDesign output object of corrected transect lines
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#'
#' @return Transect summary data frame
#'
#' @export
TransSummary=function(full.data, split.design, area){

  observers=unique(as.character(full.data$Observer))

  tsum=NULL



    for (j in 1:length(observers)){


      if(area=="YKG" || area=="YKD" || area=="ACP" || area=="CRD" || area=="YKDV" || area=="KIG" || area=="BLSC"){


          obs.flown=full.data[!duplicated(full.data[c("Year","Observer", "Transect", "ctran")]),]

          obs.flown=obs.flown[obs.flown$Observer==observers[j],]


          yr=obs.flown$Year
          obs=as.character(obs.flown$Observer)
          renum=as.numeric(as.character(obs.flown$ctran))
          seat=as.character(obs.flown$Seat)


          #orig=as.numeric(as.character(obs.flown$Transect))
          orig=as.character(obs.flown$Transect)


          len=array(0,length(orig))
          strata=vector("character",length(orig))


          for (k in 1:length(renum)){

            len[k]=sum(split.design@data$len[split.design@data$SPLIT==renum[k] & split.design@data$ORIGID==orig[k]])


            #strata[k]=names(which.max(table(full.data$strat[full.data$yr==years[i] & full.data$ctran==renum[k]])))
            split.design@data$STRATNAME=as.character(split.design@data$STRATNAME)
            strata[k]=split.design@data$STRATNAME[split.design@data$SPLIT==renum[k]][1]

          } #end k


          tsum=data.frame(Year=yr, Observer=obs, Seat=seat, Original=orig, Length=len, PartOf=renum, Strata=strata)

      }
      } # end j loop



  tsum$SampledArea=.2*tsum$Length


  return(tsum)

}





#' Apply a detection correction to an index estimate
#'
#' CorrectionFactor will apply a detection correct to an index estimate.
#'
#' CorrectionFactor currently only works for dusky Canada geese (DCGO).
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param estimates The estimates list object from a Densities output list
#' @param species The character string or vector representing the species to be corrected.
#'
#' @return original estimates file with additional detection-corrected column
#'
#' @export
CorrectionFactor=function(estimates, species){

  if(species=="DCGO"){

    estimates$adjusted.ibb=0
    estimates$adjusted.ibb.se=0

    estimates$adjusted.ibb[estimates$Species=="DCGO"]=estimates$ibb[estimates$Species=="DCGO"]*3.3416
    estimates$adjusted.ibb.se[estimates$Species=="DCGO"]=sqrt(((estimates$ibb.se[estimates$Species=="DCGO"]/estimates$ibb[estimates$Species=="DCGO"])^2)+((0.3244/3.3416)^2))*estimates$adjusted.ibb[estimates$Species=="DCGO"]


  }


  return(estimates)
}



#' FixTavs runs near the end of DataSelect to overwrite species codes from CCGO to TAVS.
#'
#' FixTavs runs near the end of DataSelect to overwrite species codes from CCGO to TAVS.
#'
#' FixTavs runs near the end of DataSelect to overwrite species codes from CCGO to TAVS.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param selected.data A nearly complete DataSelect object from the Yukon Kuskokwim Delta
#'
#' @return original data frame (full.data) with some CCGO species codes changed to TAVS
#'
#' @export
FixTavs=function(selected.data){

  selected.data$obs$Species[selected.data$obs$Species == "CCGO" & selected.data$obs$Stratum == "Low"]="TAVS"
  selected.data$obs$Species[selected.data$obs$Species == "CCGO" & selected.data$obs$Stratum == "8"]="TAVS"



  selected.data$obs$Species[selected.data$obs$Lat>=63 & selected.data$obs$Species=="CCGO"]="TAVS"

  return(selected.data)

}




#' Summarize design strata for analysis
#'
#' StrataSummary will provide the overall study area spatial characteristics for analysis
#'
#' Stratasummary will compute areas of each strata (in km^2) in a design strata file as well as calculate the maximum possible transects in
#' a sample (M) using FindVoids.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param strata.file The path to the design stratification .shp file
#'
#' @return data frame summary of stratification
#'
#' @export
StrataSummary=function(strata.file){


  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.file, type="proj")
  strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84 +datum=NAD83")

  strata.proj@data$AREA=raster::area(strata.proj)

  #Get actual areas from gis layers
  strata.area=aggregate(strata.proj@data$AREA~strata.proj@data$STRATNAME, FUN=sum)
  colnames(strata.area)=c("strata", "layer.area")

  #Convert from m^2 to km^2
  strata.area$layer.area=strata.area$layer.area / 1000000


  strata.proj@data$id = rownames(strata.proj@data)
  strata.fort <- ggplot2::fortify(strata.proj, region="STRATNAME")
  strata.df=plyr::join(strata.fort, strata.proj@data, by="id")

  ##extract min and max lat from shp.df, calc gcd and / by sampled width
  min.lat=aggregate(strata.df$lat~strata.df$id, FUN=min)
  max.lat=aggregate(strata.df$lat~strata.df$id, FUN=max)
  piece.min.lat=aggregate(strata.df$lat~strata.df$piece+strata.df$id, FUN=min)
  colnames(piece.min.lat)=c("piece", "id", "min")
  piece.max.lat=aggregate(strata.df$lat~strata.df$piece+strata.df$id, FUN=max)
  colnames(piece.max.lat)=c("piece", "id", "max")

  pieces=data.frame("id"=piece.max.lat$id, "min"=piece.min.lat$min, "max"=piece.max.lat$max)
  pieces=pieces[order(pieces$id, -pieces$max),]

  #Find holes between shape polygons
  voids=FindVoids(pieces=pieces)

  #111.5 km in 1 deg lat

  diff.lat=data.frame("strata"=min.lat[,1], "diff"=abs(max.lat[,2]-min.lat[,2])*111.5)

  #If there are voids in strata, remove them from possible sample area

  diff.lat$strata=as.character(diff.lat$strata)

  for (i in 1:length(diff.lat$strata)){
    if (diff.lat$strata[i] %in% voids$id){diff.lat$diff[i]=diff.lat$diff[i]-111.5*voids$d[voids$id==diff.lat$strata[i]]}

  }


  #Total possible transects available (M)
  diff.lat$M=diff.lat$diff/(.2)

 strata.area=merge(strata.area, diff.lat)

 if("Egg Island" %in% strata.area$strata || "egg" %in% strata.area$strata){

   strata.area$diff[strata.area$strata=="Egg Island" || strata.area$strata=="egg"]=10
   strata.area$M[strata.area$strata=="Egg Island" || strata.area$strata=="egg"]=50


 }

  return(strata.area)
}



#' Trim observations that fall outside of the area of inference
#'
#' TrimToStrata will clip observation data to the strata polygons
#'
#' TrimToStrata is especially important on the Copper River Delta (CRD) study area, where design transects intentionally sample outside
#' of the area of inference.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file of observations
#' @param strata.path The path to the design stratification .shp file
#'
#' @return data frame of full.data without egregious observations
#'
#' @export
TrimToStrata=function(full.data, strata.path){


  sp::coordinates(full.data)=~Lon+Lat
  sp::proj4string(full.data)=sp::CRS("+proj=longlat +ellps=WGS84 +datum=NAD83")

  strata.proj=LoadMap(strata.path, type="proj")
  strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84 +datum=NAD83")

  full.data=raster::intersect(full.data, strata.proj)

  return(as.data.frame(full.data))


}

#' Combine multiple observers' transcribed files in a given year
#'
#' Combine multiple observers' transcribed files in a given year
#'
#' PoolData uses MasterFileList to iterate through and appropriately pool observers in certain years
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param year The year to pool
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  }
#'
#'  @return A .csv file is written to the data directory of pooled observations
#'
#' @export
PoolData=function(year, area){

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]

  data.path=NULL

  for (i in 1:length(entries[,1])){

  data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")

  if(i==1){data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)}

  if(i!=1){
    temp=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)
    data=rbind(data, temp)
    }

  }

  if(area != "YKD"){

  LF=data[data$Seat=="LF",]
  RF=data[data$Seat=="RF",]

  LF$Observer=paste(unique(LF$Observer), collapse="_")
  RF$Observer=paste(unique(RF$Observer), collapse="_")

  write.LF=paste(dirname(data.path),"/", area, "_", year, "_QCObs_SeatLF.csv", sep="")
  write.RF=paste(dirname(data.path),"/", area, "_", year, "_QCObs_SeatRF.csv", sep="")


  write.csv(LF, write.LF, quote=FALSE, row.names=FALSE )
  write.csv(RF, write.RF, quote=FALSE, row.names=FALSE )
  }

  if(area=="YKD"){

    data$Observer=paste(unique(data$Observer), collapse="_")
    data$Seat=paste(unique(data$Seat), collapse="_")

    write.pool=paste(dirname(data.path),"/", area, "_", year, "_QCObs_Pooled.csv", sep="")
    write.csv(data, write.pool, quote=FALSE, row.names=FALSE )


  }

}


#' Provide transect-level summaries for a given species
#'
#' SepciesTransect will summarize spatial and observation information for a range of years by Obs_Type
#'
#' SpeciesTransect is used to provide a full record of a species on an area for a range of years, by transect and strata.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  }
#' @param year The range of years
#' @param species The (usually 4 character) species code of the desired species
#' @param strata.overwrite forced overwrite of stratification information
#'
#' @return List object with 3 elements: \enumerate{
#' \item output.table summary table of species occurrence
#' \item M.table summary of stratification characteristics
#' \item output.flkdrake flkdrake-specific information
#' }
#'
#' @export
SpeciesTransect=function(area, year, species, strata.overwrite="none", method="repo"){

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]

  now.skip=0
  rep=1

  for (i in 1:length(entries[,1])){

    if(entries$YEAR[i]==now.skip){next}

    if(entries$COMBINE[i]==1){

      if(area=="YKD" || area=="YKDV" || area=="KIG"){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKD_2001_QCObs_Pooled.csv", sep="")
        now.skip=entries$YEAR[i]
      }

      if(area=="ACP" & rep==1){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatLF.csv", sep="")
      }

      if(area=="ACP" & rep==2){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatLF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1986_QCObs_SeatLF.csv", sep="")}
      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatRF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1986_QCObs_SeatRF.csv", sep="")}
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatLF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1989_QCObs_SeatLF.csv", sep="")}


      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatRF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1989_QCObs_SeatRF.csv", sep="")}

        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatLF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1997_QCObs_SeatLF.csv", sep="")}


      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatRF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_1997_QCObs_SeatRF.csv", sep="")}

        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatLF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_2005_QCObs_SeatLF.csv", sep="")}


      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatRF.csv", sep="")
        if(method=="repo"){data.path=paste(entries$DRIVE[i], "/YKG_2005_QCObs_SeatRF.csv", sep="")}

        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="CRD" & rep==1 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }
      if(area=="CRD" & rep==1 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(rep==1){rep=2}
      if(rep==3){rep=1}
    }

    if(entries$COMBINE[i]!=1){data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")}

    strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")

    if(strata.overwrite != "none"){
      strata.path=strata.overwrite
    }

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    data=DataSelect(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path)

    #est=Densities(data, area=entries$AREA[i])

    if(i==1){

      if(species %in% unique(data$transect$Species)){
        output.table=data$transect[data$transect$Species %in% species,]
      names(output.table)[names(output.table) == "area"] <- "obs.area"
      output.table$strata.area=0

      output.flkdrake=data$obs[1,]
      output.flkdrake$strata="None"
      output.flkdrake$area=area
      output.flkdrake$obs.area=0
      output.flkdrake$strata.area=0


      if("flkdrake" %in% unique(data$obs$Obs_Type)){

        new.flkdrake=data$obs[data$obs$Obs_Type=="flkdrake", ]
        new.flkdrake$strata="None"
        new.flkdrake$area=area
        new.flkdrake$obs.area=0
        new.flkdrake$strata.area=0
        output.flkdrake=rbind(output.flkdrake, new.flkdrake)
      }
      }


      if(!(species %in% unique(data$transect$Species))){

        output.table=expand.grid(Year=data$transect$Year[1], Observer=data$transect$Observer[1], Species=species, Obs_Type=unique(data$transect$Obs_Type), ctran=unique(data$transect$ctran), Num=0, itotal=0, total=0, ibb=0, sing1pair2=0, flock=0)
        output.table$strata.area=0


        for(u in 1:length(output.table$strata.area)){
          output.table$obs.area[u]=data$transect$area[data$transect$ctran==output.table$ctran[u]][1]
        output.table$strata[u]=data$transect$strata[data$transect$ctran==output.table$ctran[u]][1]
        }

        output.flkdrake=data$obs[1,]
        output.flkdrake$strata="None"
        output.flkdrake$area=area
        output.flkdrake$obs.area=0
        output.flkdrake$strata.area=0

      }


      for(j in 1:length(output.table$strata.area)){
        output.table$strata.area[j]=data$strata$layer.area[output.table$strata[j]==data$strata$strata]
      }

      data$strata$Year=entries$YEAR[i]
      M.table=data$strata

        }

    if(i>1){

      if(species %in% unique(data$transect$Species)){
      temp.table=data$transect[data$transect$Species %in% species,]
      names(temp.table)[names(temp.table) == "area"] <- "obs.area"
      temp.table$strata.area=0

      if("flkdrake" %in% unique(data$obs$Obs_Type)){

        new.flkdrake=data$obs[data$obs$Obs_Type=="flkdrake", ]

        new.flkdrake$strata="None"
        new.flkdrake$area=area
        new.flkdrake$obs.area=0
        new.flkdrake$strata.area=0


        output.flkdrake=rbind(output.flkdrake, new.flkdrake)
      }

      }


      if(!(species %in% unique(data$transect$Species))){

        temp.table=expand.grid(Year=data$transect$Year[1], Observer=data$transect$Observer[1], Species=species, Obs_Type=unique(data$transect$Obs_Type), ctran=unique(data$transect$ctran), Num=0, itotal=0, total=0, ibb=0, sing1pair2=0, flock=0)
        temp.table$strata.area=0

        for(v in 1:length(temp.table$strata.area)){
        temp.table$obs.area[v]=data$transect$area[data$transect$ctran==temp.table$ctran[v]][1]
        temp.table$strata[v]=data$transect$strata[data$transect$ctran==temp.table$ctran[v]][1]
        }

      }



      for(j in 1:length(temp.table$strata.area)){
        temp.table$strata.area[j]=data$strata$layer.area[temp.table$strata[j]==data$strata$strata]
      }

      data$strata$Year=entries$YEAR[i]
      output.table=rbind(output.table, temp.table)
      M.table=rbind(M.table, data$strata)
   }

  }

  output.table$area=area
  output.table$freq=output.table$Num

  output.table=output.table[!(output.table$Obs_Type=="flkdrake" & output.table$Num > 0),]

  output.flkdrake=output.flkdrake[output.flkdrake$Obs_Type=="flkdrake" & output.flkdrake$Species %in% species,]

  if(length(output.flkdrake[,1]>=1)){
  output.flkdrake$freq=0

  sum.flkdrake = aggregate(freq~Year+Observer+Species+Obs_Type+ctran+Num+obs.area+strata+strata.area+area, data=output.flkdrake, FUN = length)

  for(n in 1:length(sum.flkdrake$Year)){

    sum.flkdrake$obs.area[n]=output.table$obs.area[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]
    sum.flkdrake$strata[n]=output.table$strata[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]
    sum.flkdrake$strata.area[n]=output.table$strata.area[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]


  }

  }

  output.table = output.table[,!names(output.table)%in%c("itotal", "total", "ibb", "sing1pair2", "flock")]



  if(length(output.flkdrake[,1]>=1)){
  output.table=rbind(output.table, sum.flkdrake)
  }

  output.table=output.table[order(output.table$Year, output.table$Observer, output.table$Species, output.table$ctran, output.table$Obs_Type),]

  return(list(output.table, M.table, output.flkdrake))


}



#' Add large or small flock information
#'
#' Add large or small flock information
#'
#' This function was added for detection estimation of flocks based on flock size.  It adds a class column to the full data that
#' specifies small or large flocks.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file of observations
#'
#' @return data frame of full.data with additional column of class
#'
#' @export
AddClass=function(full.data){

  full.data$class="empty"

  for (i in 1:length(full.data$class)){

  if(full.data$Obs_Type[i]=="single"){full.data$class[i]="single"}

  if(full.data$Obs_Type[i]=="pair"){full.data$class[i]="pair"}

  if(full.data$Obs_Type[i]=="flkdrake"){

    if(full.data$Num[i]==1){full.data$class[i]="single"}
    if(full.data$Num[i]==2){full.data$class[i]="pair"}
    if(full.data$Num[i]>2 && full.data$Num[i]<6){full.data$class[i]="s.flock"}
    if(full.data$Num[i]>5){full.data$class[i]="l.flock"}

  }

    if(full.data$Obs_Type[i]=="open"){

      if(full.data$Num[i]==1){full.data$class[i]="single"}
      if(full.data$Num[i]==2){full.data$class[i]="pair"}
      if(full.data$Num[i]>2 && full.data$Num[i]<6){full.data$class[i]="s.flock"}
      if(full.data$Num[i]>5){full.data$class[i]="l.flock"}

    }


  }


  return(full.data)
}


#' Iterate over all species and projects and create input files for use in StateSpace
#'
#' Iterate over all species and projects and create input files for use in StateSpace
#'
#' Iterate over all species and projects and create input files for use in StateSpace.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param folder.path The output folder to send newly generated files to
#'
#' @return Many .csv files are created in folder.path
#'
#' @export
MakeStateSpace=function(folder.path, area="all") {

  for(spec in seq_along(unique(ACPHistoric$combined$Species))){

    data=data.frame(Year=ACPHistoric$combined$Year[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]],
                    N=ACPHistoric$combined$itotal[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]],
                    SE=ACPHistoric$combined$itotal.se[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]])

    data=data[order(data$Year),]

    path=paste("T:/Frost/StateSpaceApp/Data/", unique(ACPHistoric$combined$Species)[spec], "_itotal_ACP_all.csv", sep="")

    write.csv(data, path, row.names = FALSE, quote = FALSE)



  }


  for(spec in seq_along(unique(ACPHistoric$combined$Species))){

    data=data.frame(Year=ACPHistoric$combined$Year[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009],
                    N=ACPHistoric$combined$itotal[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009],
                    SE=ACPHistoric$combined$itotal.se[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009])

    data=data[order(data$Year),]

    path=paste("T:/Frost/StateSpaceApp/Data/", unique(ACPHistoric$combined$Species)[spec], "_itotal_ACP_10.csv", sep="")

    write.csv(data, path, row.names = FALSE, quote = FALSE)



  }


  for(spec in seq_along(unique(ACPHistoric$combined$Species))){

    data=data.frame(Year=ACPHistoric$combined$Year[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]],
                    N=ACPHistoric$combined$ibb[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]],
                    SE=ACPHistoric$combined$ibb.se[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec]])

    data=data[order(data$Year),]

    path=paste("T:/Frost/StateSpaceApp/Data/", unique(ACPHistoric$combined$Species)[spec], "_ibb_ACP_all.csv", sep="")

    write.csv(data, path, row.names = FALSE, quote = FALSE)



  }


  for(spec in seq_along(unique(ACPHistoric$combined$Species))){

    data=data.frame(Year=ACPHistoric$combined$Year[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009],
                    N=ACPHistoric$combined$ibb[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009],
                    SE=ACPHistoric$combined$ibb.se[ACPHistoric$combined$Species==unique(ACPHistoric$combined$Species)[spec] & ACPHistoric$combined$Year > 2009])

    data=data[order(data$Year),]

    path=paste("T:/Frost/StateSpaceApp/Data/", unique(ACPHistoric$combined$Species)[spec], "_ibb_ACP_10.csv", sep="")

    write.csv(data, path, row.names = FALSE, quote = FALSE)



  }
}





SpeciesObs=function(area, year, species, strata.overwrite="none"){

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]

  now.skip=0
  rep=1

  for (i in 1:length(entries[,1])){

    if(entries$YEAR[i]==now.skip){next}

    if(entries$COMBINE[i]==1){

      if(area=="YKD" || area=="YKDV" || area=="KIG"){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKD_2001_QCObs_Pooled.csv", sep="")
        now.skip=entries$YEAR[i]
      }

      if(area=="ACP" & rep==1){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatLF.csv", sep="")
      }

      if(area=="ACP" & rep==2){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="CRD" & rep==1 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }
      if(area=="CRD" & rep==1 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(rep==1){rep=2}
      if(rep==3){rep=1}
    }

    if(entries$COMBINE[i]!=1){data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")}

    strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")

    if(strata.overwrite != "none"){
      strata.path=strata.overwrite
    }

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    data=DataSelect(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path)


    if(i==1){

    output.table=data$obs[1,]
    output.table=output.table[-1,]

    if(any(data$obs$Species %in% species)){

      temp.table=data$obs[data$obs$Species %in% species,]

      output.table=rbind(output.table, temp.table)

    }

    }

    if(i>1){

      if(any(data$obs$Species %in% species)){

        temp.table=data$obs[data$obs$Species %in% species,]

        output.table=rbind(output.table, temp.table)
    }

  }
  }

  return(output.table)


}




CombineByStrata=function(counts.final1, counts.final2){

  estimates=rbind(counts.final1, counts.final2)
  strata.list=unique(estimates$strata)
  sp.list=unique(estimates$Species)

  combined=data.frame(Year=rep(counts.final1$Year[1], each=length(strata.list)*length(sp.list)), Species=rep(sp.list, each=length(strata.list)*length(sp.list)), strata=strata.list, total=0, total.var=0, total.se=0, itotal=0, itotal.var=0, itotal.se=0, ibb=0, ibb.var=0, ibb.se=0, sing1pair2=0, sing1pair2.var=0, sing1pair2.se=0, flock=0, flock.var=0, flock.se=0)

  for(i in 1:length(combined$strata)){


    combined$total[i]=mean(estimates$total.est[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])

    combined$itotal[i]=mean(estimates$itotal.est[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])

    combined$ibb[i]=mean(estimates$ibbtotal.est[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])

    combined$sing1pair2[i]=mean(estimates$sing1pair2.est[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])

    combined$flock[i]=mean(estimates$flock.est[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])

    combined$total.var[i]=sum(estimates$var.N[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.N[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])^2)

    combined$itotal.var[i]=sum(estimates$var.Ni[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Ni[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])^2)

    combined$ibb.var[i]=sum(estimates$var.Nib[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nib[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])^2)

    combined$sing1pair2.var[i]=sum(estimates$var.Nsing1pair2[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nsing1pair2[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])^2)

    combined$flock.var[i]=sum(estimates$var.Nflock[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nflock[estimates$strata==combined$strata[i] & estimates$Species==combined$Species[i]])^2)

  }

  combined$total.se=sqrt(combined$total.var)
  combined$itotal.se=sqrt(combined$itotal.var)
  combined$ibb.se=sqrt(combined$ibb.var)
  combined$sing1pair2.se=sqrt(combined$sing1pair2.var)
  combined$flock.se=sqrt(combined$flock.var)

  combined[is.na(combined)]=0

  return(combined)

}








#' Update the 7 .rda objects for streamlined package function
#'
#' Update the 7 .rda objects for streamlined package function
#'
#' This function was added to streamline any updates to the 7 package objects that define the core inputs for estimate generation.  The
#' 7 objects are:
#' \itemize{
#'  \item MasterFileList - A list of each year/area/strata/transect/observer combination for the ACP, CRD, YKD, YKG, and 10-02 surveys and the
#'  location of all associated clean input files that produce an index estimate.
#'  \item MasterFileList_WBPHS - A list of each year/observer combination for the WBPHS surve and the
#'  location of all associated clean input files that produce an index estimate.
#'  \item sppntable - A list of the accepted species code, common name, and scientific name of all avian species used in the package.  These are
#'  separated by project.
#'  \item WBPHSsppntable - A list of the accepted species code, common name, and scientific name of all avian species used on the WBPHS survey.
#'  \item WBPHS_VCF - A list of visibility correction factors (VCFs) by species and stratum for the WBPHS survey.
#'  \item WBPHS_PoolSpecies - A list of species and selected associated guilds (eider, grebe, merganser, scoter) for pooling WBPHS estimates.
#'  \item WBPHSHistoric - The master table of historic WBPHS estimates.
#'  }
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param input.path The path to the folder containing the .csv data to convert to .rda objects.
#' @param output.path The path to the R package data folder to hold the updated objects.
#'
#' @return The 7 objects are updated and saved in the data folder of the package.
#'
#' @export
UpdateAllObjects=function(input.path, output.path){

  MasterFileList=read.csv(paste(input.path, "/AerialSurveyFileMatrix.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  MasterFileList_WBPHS=read.csv(paste(input.path, "/AerialSurveyFileMatrix_WBPHS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  sppntable=read.csv(paste(input.path, "/AKAerialSpeciesAOUCodes.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  WBPHSsppntable=read.csv(paste(input.path, "/AKAerialWBPHSAOUCodes.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  WBPHS_VCF=read.csv(paste(input.path, "/WBPHS_VCF.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  WBPHS_PoolSpecies=read.csv(paste(input.path, "/WBPHS_PoolSpecies.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  WBPHSHistoric=read.csv(paste(input.path, "/EstimatesTableWBPHS.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)


  save(MasterFileList, file=paste(output.path, "/MasterFileList.rda", sep=""))
  save(MasterFileList_WBPHS, file=paste(output.path, "/MasterFileList_WBPHS.rda", sep=""))
  save(sppntable, file=paste(output.path, "/sppntable.rda", sep=""))
  save(WBPHSsppntable, file=paste(output.path, "/WBPHSsppntable.rda", sep=""))
  save(WBPHS_VCF, file=paste(output.path, "/WBPHS_VCF.rda", sep=""))
  save(WBPHS_PoolSpecies, file=paste(output.path, "/WBPHS_PoolSpecies.rda", sep=""))
  save(WBPHSHistoric, file=paste(output.path, "/WBPHSHistoric.rda", sep=""))



}




#' Update one of the 5 tables of historic estimates (.rda objects ACPHistoric, CRDHistoric, YKDHistoric, YKGHistoric, WBPHSHistoric)
#'
#' Update one of the historic estimates tables with one year of new data
#'
#' This function updates the historic tables (available as package data .rda objects) with one year of new data.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item YKDV - Yukon Kuskokwim Delta, ducks, visibility study strata
#'  }
#' @param year The year of estimates to add
#'
#' @return The associated object is updated and saved in the data folder of the package.
#'
#' @export
AppendYear = function(area, year){

  update=EstimatesTable(area=area, year=year)
  update$expanded.table$sha=system("git rev-parse HEAD", intern=TRUE)


  if(area=="ACP"){
    ACPHistoric$output.table=rbind(ACPHistoric$output.table, update$output.table)
    ACPHistoric$expanded.table=rbind(ACPHistoric$expanded.table, update$expanded.table)
    ACPHistoric$combined=rbind(ACPHistoric$combined, update$combined)
    save(ACPHistoric, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/ACPHistoric.rda")

  }

  if(area=="CRD"){
    CRDHistoric$output.table=rbind(CRDHistoric$output.table, update$output.table)
    CRDHistoric$expanded.table=rbind(CRDHistoric$expanded.table, update$expanded.table)
    CRDHistoric$combined=rbind(CRDHistoric$combined, update$combined)
    save(CRDHistoric, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/CRDHistoric.rda")

  }

  if(area=="YKD"){
    YKDHistoric$output.table=rbind(YKDHistoric$output.table, update$output.table)
    YKDHistoric$expanded.table=rbind(YKDHistoric$expanded.table, update$expanded.table)
    YKDHistoric$combined=rbind(YKDHistoric$combined, update$combined)
    save(YKDHistoric, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/YKDHistoric.rda")

  }

  if(area=="YKG"){
    YKGHistoric$output.table=rbind(YKGHistoric$output.table, update$output.table)
    YKGHistoric$expanded.table=rbind(YKGHistoric$expanded.table, update$expanded.table)
    YKGHistoric$combined=rbind(YKGHistoric$combined, update$combined)
    save(YKGHistoric, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/YKGHistoric.rda")

  }

  if(area=="YKDV"){
    YKDVHistoric$output.table=rbind(YKDVHistoric$output.table, update$output.table)
    YKDVHistoric$expanded.table=rbind(YKDVHistoric$expanded.table, update$expanded.table)
    YKDVHistoric$combined=rbind(YKDVHistoric$combined, update$combined)
    save(YKDVHistoric, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/YKDVHistoric.rda")

  }


}




#' Output .csv files of historic estimates
#'
#' Output a range of annual estimates for a given aerial survey
#'
#' This function will output .csv files representing estimates from the chosen survey, year(s), and species.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item YKDV - Yukon Kuskokwim Delta, ducks, visibility study strata
#'  }
#' @param year The years of estimates to add
#' @param species The species requested (defaults to all)
#' @param output.folder The folder path for the resulting 3 .csv files
#'
#'
#' @return The tables are generated and written to the output folder.
#'
#' @export
WriteResults = function(area, year="all", species = "all", output.folder){

  if(area=="YKD"){data=YKDHistoric}
  if(area=="YKG"){data=YKGHistoric}
  if(area=="ACP"){data=ACPHistoric}
  if(area=="CRD"){data=CRDHistoric}
  if(area=="YKDV"){data=YKDVHistoric}

  if(year != "all"){
    data$output.table = data$output.table %>%
      filter(Year %in% year)
    data$expanded.table = data$expanded.table %>%
      filter(Year %in% year)
    data$combined = data$combined %>%
      filter(Year %in% year)

  }

  if(species != "all"){
    data$output.table = data$output.table %>%
      filter(Species %in% species)
    data$expanded.table = data$expanded.table %>%
      filter(Species %in% species)
    data$combined = data$combined %>%
      filter(Species %in% species)

  }

  outfile1 = paste(area, min(data$combined$Year), "to", max(data$combined$Year), sep="")

  outfile2 = paste(outfile1, "Combined.csv", sep="")
  write.csv(data$combined, paste(output.folder, outfile2, sep=""), quote=FALSE, row.names=FALSE)

  outfile2 = paste(outfile1, "Expanded.csv", sep="")
  write.csv(data$expanded.table, paste(output.folder, outfile2, sep=""), quote=FALSE, row.names=FALSE)

  outfile2 = paste(outfile1, "Output.csv", sep="")
  write.csv(data$output.table, paste(output.folder, outfile2, sep=""), quote=FALSE, row.names=FALSE)



}
