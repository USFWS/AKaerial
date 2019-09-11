
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


library(rgdal)
library(ggplot2)
library(maptools)
library(rgeos)
library(plyr)
library(tools)
library(mapproj)
library(raster)
library(zoo)
library(ggspatial)
library(outliers)
library(rmarkdown)
library(kableExtra)
library(leaflet)
library(RColorBrewer)
library(smoothr)
library(lwgeom)
library(dplyr)
library(tidyr)



DataSelect <- function(area, data.path=NA, strata.path=NA, transect.path=NA){



  if(area=="YKDV" || area=="KIG"){area="YKD"}

  data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)

  data=data[data$Code==1,]

  data$Obs_Type[data$Obs_Type=="open" & data$Num==1 & data$Species!="SWANN"]="single"

  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  data=CorrectTrans(full.data=data, area=area, split.design=split.design, strata.file=strata.path)

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





CorrectUnit=function(full.data){

  acceptable=c("open", "single", "pair","flkdrake")

  for (i in 1:length(full.data$unit)){

    if(full.data$unit[i] %in% acceptable){next}

    print(paste("Nonsense detected.  Unit ", full.data$unit[i], " is not acceptable."))


  }

  full.data=full.data[full.data$unit %in% acceptable,]
  return(full.data)

}







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



    agg=as.data.frame(complete(data=agg, Year, Observer, Species, Obs_Type, ctran, fill=list(Num=0, itotal=0, total=0, ibb=0, sing1pair2=0, flock=0)))

  agg$area=0

  agg$strata="none"


  selected.data$flight$Strata=as.character(selected.data$flight$Strata)

  for(g in 1:length(agg$area)){

    agg$area[g]=sum(selected.data$flight$SampledArea[selected.data$flight$Year==agg$Year[g] & selected.data$flight$Observer==agg$Observer[g] & selected.data$flight$PartOf==agg$ctran[g]])
    agg$strata[g]=selected.data$flight$Strata[selected.data$flight$Year==agg$Year[g] & selected.data$flight$Observer==agg$Observer[g] & selected.data$flight$PartOf==agg$ctran[g]][1]

  }



return(agg[order(agg$Year, agg$Observer, agg$Species, as.numeric(agg$ctran), agg$Obs_Type),])

}


SplitDesign <- function(strata.file, transect.file, SegCheck=FALSE, area="other"){


  #read and project transects
  design=rgdal::readOGR(transect.file, layer=tools::file_path_sans_ext(basename(transect.file)), verbose=FALSE)
  design.proj <- sp::spTransform(design, "+proj=longlat +ellps=WGS84")
  design.proj <- smoothr::drop_crumbs(design.proj, threshold=.1)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.file, type="proj")
  design.proj <- sp::spTransform(design, "+proj=longlat +ellps=WGS84")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  # newlines = raster::intersect(design.proj, strata.proj)
  # newlines@data$id=rownames(newlines@data)
  # newlines.fort=fortify(newlines, region="STRAT")
  # newlines.df=join(newlines.fort, newlines@data, by="id")

  #intersect transects with strata, create new attribute SPLIT that is a unique
  #numbering system for latitude/strata combos

  newlines = suppressWarnings(raster::intersect(design.proj, strata.proj))
  newlines@data$len=sp::SpatialLinesLengths(newlines, longlat=TRUE)
  newlines=smoothr::drop_crumbs(newlines, threshold=.1)
  newlines@data$id=rownames(newlines@data)
  newlines.proj=sp::spTransform(newlines, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  midpoints=as.data.frame(sp::spTransform(maptools::SpatialLinesMidPoints(newlines.proj), "+proj=longlat +ellps=WGS84"))
  newlines.fort=ggplot2::fortify(newlines, region="STRAT")
  newlines.df=plyr::join(newlines.fort, newlines@data, by="id")
  newlines.df$ROUNDED=round(newlines.df$lat, digits=3)


  if(area=="CRD"){

    for(i in 1:length(newlines.df$STRATNAME)){
    if(newlines.df$STRATNAME[i]=="Egg Island"){newlines.df$ROUNDED[i]= round(newlines.df$long[i], digits=2)}
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

    factpal=colorFactor(brewer.pal(n=length(unique(strata.proj$STRATNAME)), name="Spectral"), as.factor(strata.proj$STRATNAME))


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





LoadMap <- function(map.file, type="df") {



  maptools::gpclibPermit()
  strata <- rgdal::readOGR(map.file, layer=tools::file_path_sans_ext(basename(map.file)), verbose=FALSE)

  strata.proj <- sp::spTransform(strata, "+proj=longlat +ellps=WGS84")
  strata<- rgeos::gBuffer(strata, byid=TRUE, width=0)



  strata.proj@data$id = rownames(strata.proj@data)

  #ifelse(area=="acp", strata.fort <- fortify(strata.proj, region="STRATNAME"), strata.fort <- fortify(strata.proj, region="STRAT"))
  strata.fort <- ggplot2::fortify(strata.proj, region="id")

  strata.df=plyr::join(strata.fort, strata.proj@data, by="id")

  if(type=="df") {return(strata.df)}
  if(type=="proj") {return(strata.proj)}
}


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
    trans.proj <- sp::spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

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
    else if(full.data$Obs_Type[i]=="flkdrake" & full.data$Num[i]<5){
      full.data$itotal[i]=2*full.data$Num[i]
      full.data$total[i]=full.data$Num[i]
      full.data$ibb[i]=2*full.data$Num[i]
      full.data$sing1pair2[i]=0
      full.data$flock[i]=full.data$Num[i]

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

PointsToStrata=function(full.data, area){


  #full.data=SpatialNA(full.data)

  x=na.approx(full.data$long, na.rm=FALSE)
  y=na.approx(full.data$lat, na.rm=FALSE)


  sp=cbind(x,y)

  sp=SpatialPoints(sp)

  proj4string(sp)=CRS("+proj=longlat +ellps=WGS84")

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


TranSelect = function(year, area){

  if(area=="ykg"){

  trans=list("file"="YKG_2018_MemoTrans.shp", "layer"="YKG_2018_MemoTrans")
  }

  if(area=="crd"){

    trans=list("file"="CRD_2018_Transects.shp", "layer"="CRD_2018_Transects")
    return(trans)
    }


  if(area=="acp"){

    trans=list("file"="ACP_2018_Transects.shp", "layer"="ACP_2018_Transects")
  }



  return(trans)
}







Densities=function(data, n.obs=1, trans.method="gis", trans.width=.2, area, output=FALSE) {

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


  #Make sure totals are numeric
  #t3$total=as.numeric(t3$total)
  #t3$itotal=as.numeric(t3$itotal)
  #t3$ibb=as.numeric(t3$ibb)
  #t3$sing1pair2=as.numeric(t3$sing1pair2)


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

  for (j in 1:length(counts.final$Species)){

    M=data$strata$M[data$strata$Year==counts.final$Year[j] & data$strata$Observer==counts.final$Observer[j] & data$strata$strata==counts.final$strata[j]]
    m=data$strata$m[data$strata$Year==counts.final$Year[j] & data$strata$Observer==counts.final$Observer[j] & data$strata$strata==counts.final$strata[j]]
    prop.m=((1-(m/M))/m)

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




MakeZeroes=function(full.data){

  #Appends a count of 0 to transects that were flown but did not record any of a given species

  #list of years
  year.list=as.numeric(unique(full.data$yr))
  #list of observers
  obs.list=as.character(unique(full.data$obs))
  #list of transects
  tran.list=as.character(unique(full.data$ctran))
  #list of species
  sp.list=as.character(unique(full.data$sppn))


  #cycle through, check each transect/observer combo for each species
  for (h in 1:length(year.list)){
    print(paste("Making zeroes for ", year.list[h]))
  for (i in 1:length(sp.list)){
    for (j in 1:length(tran.list)){
      for (k in 1:length(obs.list)){

  #skip if count exists
      if(any(as.character(full.data$sppn)==sp.list[i] & as.character(full.data$ctran)==tran.list[j] & as.character(full.data$obs)==obs.list[k] & full.data$yr==year.list[h]))
      {next}

  #make sure that transect was flown
      if(any(as.character(full.data$obs)==obs.list[k] & as.character(full.data$ctran)==tran.list[j] & full.data$yr==year.list[h]))
      {
  #add the 0 row
        new.row=c(year.list[h], NA, NA, NA, obs.list[k], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list[i], 0, "open", NA, NA, tran.list[j], NA, 0)
      full.data=rbind(full.data,new.row)
      }
      } #end obs

  } #end tran
  } #end sp
  } #end year

  return(full.data)

}






TransectTable <- function(trans.file, trans.layer, obs=1, method, area) {


  if(method=="gis" & area != "acp"){

    #split the design shp file into component segments (file has each transect cutting through
    #multiple strata and keeping same number)

    trans.points=SplitDesign(file.name=trans.file, layer.name=trans.layer, area=area)

    id=trans.points$id

    x=trans.points$long
    y=trans.points$lat



    }

  if(method=="gis" & area == "acp"){


    trans.obj=rgdal::readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- sp::spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

    strata.proj=LoadMap(area, type="proj")

    newlines = raster::intersect(trans.proj, strata.proj)
    newlines@data$id=rownames(newlines@data)
    newlines.fort=ggplot2::fortify(newlines, region="STRAT")
    trans.points=plyr::join(newlines.fort, newlines@data, by="id")

    id=trans.points$id
    x=vector("numeric",length=2*length(unique(trans.points$id)))
    y=vector("numeric",length=2*length(unique(trans.points$id)))
    id=unique(trans.points$id)

    original=vector("character",length=length(unique(trans.points$id)))
    renum=vector("character",length=length(unique(trans.points$id)))
    type=vector("character",length=length(unique(trans.points$id)))

    for(i in 1:length(id)){
      x[2*i-1]=trans.points$long[trans.points$id==id[i] & trans.points$order==1]
      x[2*i]=trans.points$long[trans.points$order==which.max(trans.points$order[trans.points$id==id[i]]) & trans.points$id==id[i]]
      y[2*i-1]=trans.points$lat[trans.points$id==id[i] & trans.points$order==1]
      y[2*i]=trans.points$lat[trans.points$order==which.max(trans.points$order[trans.points$id==id[i]]) & trans.points$id==id[i]]
     original[i]=trans.points$ORIGID[trans.points$id==id[i] & trans.points$order==1]
     renum[i]=as.character(trans.points$OBJECTID[trans.points$id==id[i] & trans.points$order==1])
     type[i]=trans.points$STRAT[trans.points$id==id[i] & trans.points$order==1]


     }




  }





  #pulls in design file that is a list of start and end points

  if(method=="text"){
  trans.points=trans.file
  code=as.character(trans.points$ident)
  id=code
  side=code
  original=NULL

  for (i in 1:length(unique(code))){

      id[i]=as.numeric(substr(code[i],nchar(code[i])-2, nchar(code[i])-1))
      side[i]=substr(code[i], nchar(code[i]), nchar(code[i]))


  }
  }

  #either method above results in x,y and the following code calculates great circle distance
  #for each set of coordinates


  width=obs*.2


  id=as.numeric(id)

  d=vector("numeric",length=length(unique(id)))

  #type=vector("character", length=length(unique(id)))

  sp=cbind(x,y)
  sp.which=seq(1,length(x), by=2)
  sp.set=data.frame(x=((sp[sp.which,1]+sp[sp.which+1,1])/2), y=sp[sp.which,2])


  sp.set=SpatialPoints(sp.set)
  proj4string(sp.set)=CRS("+proj=longlat +ellps=WGS84")


  if(method=="text"){type=over(sp.set,LoadMap(area, type="proj"))$STRATNAME}
  #if(method=="gis"){type=trans.points$STRAT[seq(1,length(trans.points$STRAT), by=2)]}

  #gcd.slc works best for a matrix of coords
  for (i in 1:length(d)){

  coords=matrix(c(x[2*i-1], x[2*i], y[2*i-1], y[2*i]), nrow=2, ncol=2)


  d[i]=gcd.slc(coords[1,1], coords[1,2], coords[2,1], coords[2,2])


  }


  output=data.frame(oldid=original, newid=renum, d=d, type=type, des.area=width*d)
  output=output[output$d>0.25,]



  if(method=="text"){output$original=output$id}


  #output=output[output$d>0.5,]
  print(output)
  return(output)

  }


#converts degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  long1=deg2rad(long1)
  lat1=deg2rad(lat1)
  long2=deg2rad(long2)
  lat2=deg2rad(lat2)

  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}


#necessary for the calculation of the maximum available transect calculation (M) component of
#the variance equation when strata are separate polygons in a shp file.  Calculates the distance
#between discontinuous pieces and removes it from possible sampled area.

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


TransectLevel=function(data, n.obs=2, trans.method="gis", trans.width=.2, area) {

  trans=TranSelect(year=data$yr[1], area=area)

  trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
  trans.layer=trans$layer
  shp=LoadMap(area=area,type="proj")

  #Save old warning settings to revert to before leaving function
  oldw <- getOption("warn")
  #Suppress spatial warnings inside function call
  options(warn=-1)


  data$strat=as.character(data$strat)

  #Compile the length (d), strata (type), area covered (des.area), and original trans name in data (original)
  #Splits transects by strata if needed
  transects=TransectTable(trans.file=trans.file, trans.layer=trans.layer, method=trans.method, area=area, obs=n.obs)
  #transects=transects[,-1]


  #Compute the total/indicated total for the group sizes indicated in the data
  adj.counts=AdjustCounts(data)

  #Sum the counts by combinations of species/transect
  counts.t=CountsTable(adj.counts)

  return(counts.t)
}


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


coordinates(full.data)=~Lon+Lat

proj4string(full.data)=CRS("+proj=longlat +ellps=WGS84")


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
  full.data$closest[j]=as.numeric(as.character(split.design$SPLIT[which.min(suppressWarnings(gDistance(full.data[j,],split.design,byid=TRUE)))]))
  full.data$dist[j]=min(suppressWarnings(gDistance(full.data[j,],split.design,byid=TRUE)))
}


if(area=="CRD"){


  for (k in 1:length(full.data$ctran)){
  full.data$ctran[k]=split.design$SPLIT[split.design$ORIGID==full.data$Transect[k]][1]

  }

  full.data$dist = full.data$dist/1000
  full.data=sp::spTransform(full.data, "+proj=longlat +ellps=WGS84")


}


if(area != "CRD"){

  full.data$ctran=full.data$closest

  #convert from meters to km, then re-project to lat/lon
  full.data$dist = full.data$dist/1000
  full.data=sp::spTransform(full.data, "+proj=longlat +ellps=WGS84")

  full.data$ctran[full.data$dist>threshold]=NA

}


full.data=full.data[!(is.na(full.data$ctran)), ]

ORIG.list=unique(split.design$ORIGID)

for(i in 1:length(ORIG.list)){


  suppressWarnings(if(any(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]])[!(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]]) %in% unique(full.data$ctran[full.data$Transect==ORIG.list[i]]))])){

    missing=unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]])[!(unique(split.design$SPLIT[split.design$ORIGID==ORIG.list[i]]) %in% unique(full.data$ctran[full.data$Transect==ORIG.list[i]]))]


    for(j in 1:length(missing)){

      dummy.row=full.data[1,]

      dummy.row$ctran=missing[j]
      dummy.row$Transect=ORIG.list[i]
      dummy.row$Species="START"
      dummy.row$Num=0
      #dummy.row$Lat=split.design$mid.Lat[split.design$ORIGID==ORIG.list[i]]
      #dummy.row$Lon=split.design$mid.Lon[split.design$ORIGID==ORIG.list[i]]

      full.data=rbind(full.data, dummy.row)
    }

  })


}

full.data$ctran=as.character(full.data$ctran)

return(as.data.frame(full.data))

}  #end CorrectTrans()



PlotObs=function(strata.plot, selected.data, multiyear=TRUE, labelyear=FALSE, box=FALSE, set.box=c(-9999,0,0,0)){

  if(multiyear==TRUE){

    strata.plot= strata.plot + geom_point(data=selected.data, aes(x=long, y=lat))


  }

  if(labelyear==TRUE){

    strata.plot= strata.plot + geom_text(data=selected.data, aes(x=long, y=lat, label=yr), hjust=0, vjust=0)


  }

if (box==TRUE){
  coordinates(selected.data)=~long+lat
  bound=bbox(selected.data)

  strata.plot= strata.plot + coord_map(xlim=c(bound[1,1]-.5, bound[1,2]+.5), ylim=c(bound[2,1]-.25, bound[2,2]+.25))

}

if (set.box[1]!=-9999){

  strata.plot= strata.plot + coord_map(xlim=c(set.box[1], set.box[2]), ylim=c(set.box[3], set.box[4]))

  #Barrow set.box=c(-157.5,-155,70.75,71.4)

}


print(strata.plot)
return(strata.plot)
}



TransSummary=function(full.data, split.design, area){

  observers=unique(as.character(full.data$Observer))

  tsum=NULL



    for (j in 1:length(observers)){


      if(area=="YKG" || area=="YKD" || area=="ACP" || area=="CRD" || area=="YKDV"){


        #if(length(full.data$long[full.data$obs==observers[j] & full.data$yr==years[i]])>0){


          obs.flown=full.data[!duplicated(full.data[c("Year","Observer", "Transect", "ctran")]),]

          obs.flown=obs.flown[obs.flown$Observer==observers[j],]


          yr=obs.flown$Year
          obs=as.character(obs.flown$Observer)
          renum=as.numeric(as.character(obs.flown$ctran))

          orig=as.numeric(as.character(obs.flown$Transect))
          len=array(0,length(orig))
          strata=vector("character",length(orig))


          for (k in 1:length(renum)){

            len[k]=sum(split.design@data$len[split.design@data$SPLIT==renum[k] & split.design@data$ORIGID==orig[k]])


            #strata[k]=names(which.max(table(full.data$strat[full.data$yr==years[i] & full.data$ctran==renum[k]])))
            split.design@data$STRATNAME=as.character(split.design@data$STRATNAME)
            strata[k]=split.design@data$STRATNAME[split.design@data$SPLIT==renum[k]][1]

          } #end k


          tsum=data.frame(Year=yr, Observer=obs, Original=orig, Length=len, PartOf=renum, Strata=strata)

          #temp.frame=temp.frame[order(part.of),]

          #tsum=rbind(tsum, temp.frame)

        #} # end year loop

      } # end ykg/ykd loop




#
#       if(area == "crd" || area=="acp"){
#
#
#         if(length(full.data$long[full.data$obs==observers[j] & full.data$yr==years[i]])>0){
#
#           obs.flown=full.data[!duplicated(full.data[c("yr","obs","tran","ctran")]),]
#
#           obs.flown=obs.flown[obs.flown$yr==years[i] & obs.flown$obs==observers[j],]
#
#
#           yr=obs.flown$yr
#           obs=obs.flown$obs
#           orig=as.numeric(as.character(obs.flown$tran))
#           len=array(0,length(orig))
#           strata=array(0,length(orig))
#           part.of=as.numeric(as.character(obs.flown$ctran))
#
#           for (k in 1:length(orig)){
#
#             if (years[i]>2011){
#               len[k]=sum(trans.obj@data$len[trans.obj@data$ORIGID==orig[k] & trans.obj@data$OBJECTID==part.of[k]])
#             }
#
#             if (years[i]<=2011){
#               len[k]=sum(trans.obj@data$len[trans.obj@data$OBJECTID==part.of[k]])
#             }
#
#             #strata[k]=names(sort(table(tpoints$strata[tpoints$OBJECTID==part.of[k]]),decreasing=TRUE)[1])
#             #strata[k]=names(which.max(table(tpoints$STRATNAME[tpoints$OBJECTID==part.of[k]])))
#             strata[k]=names(which.max(table(full.data$strat[full.data$yr==years[i] & full.data$ctran==part.of[k]])))
#
#
#           } #end k
#
#
#
#           temp.frame=data.frame(yr=yr, obs=obs, orig=orig, len=len, part.of=part.of, strata=strata)
#
#           temp.frame=temp.frame[order(orig),]
#
#           if (years[i]<=2011 & area=="acp"){
#
#             temp.frame=temp.frame[!duplicated(temp.frame[c("obs","len","part.of")]),]
#
#           }
#
#           tsum=rbind(tsum, temp.frame)
#
#
#         } #end if any obs/yr
#
#       } #end if not ykg

    } #end j observers

  #} #end i years

  tsum$SampledArea=.2*tsum$Length
#
#   if(area=="ykg" || area=="crd"){tsum=tsum[!duplicated(tsum[,c("yr", "obs", "part.of")]),]}
#
#   if(area=="acp"){
#
#     tsum.agg=aggregate(list("len"=tsum$len,"sampled.area"=tsum$sampled.area), by=list("yr"=tsum$yr,"obs"=tsum$obs,"part.of"=tsum$part.of,"strata"=tsum$strata), FUN=sum)
#     return(tsum.agg)
#
# }

  return(tsum)

}




TransData2=function(selected.data){

  #groupings list
  unit.list=c("single", "pair","open", "flkdrake")
  #list of years
  yr.list=as.character(unique(selected.data$flight$yr))
  #list of species
  sp.list=as.character(unique(selected.data$obs$sppn))




  #cycle through, check each transect/observer combo for each species
  for (h in 1:length(yr.list)){
    sub.data=selected.data$obs[selected.data$obs$yr==yr.list[h],]

    obs.list=unique(as.character(selected.data$flight$obs[selected.data$flight$yr==yr.list[h]]))

    #print(paste("Making zeroes for year ", yr.list[h]))
    for (i in 1:length(sp.list)){

      sub.data=sub.data[sub.data$sppn==sp.list[i],]

     for (j in 1:length(obs.list)){
        #print(paste("Observer ", obs.list[j]))
        tran.list=unique(selected.data$flight$part.of[selected.data$flight$yr==yr.list[h] & selected.data$flight$obs==obs.list[j]])

        sub.data=sub.data[as.character(sub.data$obs)==obs.list[j],]

        for (k in 1:length(tran.list)){

          sub.data=sub.data[as.character(sub.data$ctran)==tran.list[k],]

          for (m in 1:length(unit.list)){

  #skip if count exists


  if(any(as.character(selected.data$obs$sppn)==sp.list[i] & as.character(selected.data$obs$ctran)==tran.list[k] & as.character(selected.data$obs$obs)==obs.list[j] & selected.data$obs$yr==yr.list[h] & selected.data$obs$unit==unit.list[m]))
            if(any(sub.data$unit==unit.list[m]))
              {next}

  #add the 0 row
            new.row=c(yr.list[h], NA, NA, NA, obs.list[j], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list[i], 0, unit.list[m], NA, NA, tran.list[k], NA, 0)
            selected.data$obs=rbind(selected.data$obs,new.row)



          } #end unit

        } #end tran

      } #end obs
    } #end sp
  } #end yr

  selected.data$obs$grp=as.numeric(selected.data$obs$grp)


  agg=aggregate(grp~yr+obs+sppn+unit+ctran, data=selected.data$obs, FUN=sum)


  colnames(agg)=c("yr", "obs", "sppn", "unit", "ctran", "grp")


  agg$area=0

  agg$strata="none"


  for(g in 1:length(agg$area)){

    agg$area[g]=sum(selected.data$flight$sampled.area[selected.data$flight$yr==agg$yr[g] & selected.data$flight$obs==agg$obs[g] & selected.data$flight$part.of==agg$ctran[g]])
    agg$strata[g]=selected.data$flight$strata[selected.data$flight$yr==agg$yr[g] & selected.data$flight$obs==agg$obs[g] & selected.data$flight$part.of==agg$ctran[g]][1]

  }




  return(agg[order(agg$yr, agg$obs, agg$sppn, as.numeric(agg$ctran), agg$unit),])

}


CorrectionFactor=function(estimates, species){

  if(species=="DCGO"){

    estimates$adjusted.ibb=0
    estimates$adjusted.ibb.se=0

    estimates$adjusted.ibb[estimates$Species=="DCGO"]=estimates$ibb[estimates$Species=="DCGO"]*3.3416
    estimates$adjusted.ibb.se[estimates$Species=="DCGO"]=sqrt(((estimates$ibb.se[estimates$Species=="DCGO"]/estimates$ibb[estimates$Species=="DCGO"])^2)+((0.3244/3.3416)^2))*estimates$adjusted.ibb[estimates$Species=="DCGO"]


  }


  return(estimates)
}



FixTavs=function(selected.data){


  selected.data$flight$Strata=as.character(selected.data$flight$Strata)
  Lows=unique(selected.data$flight$PartOf[selected.data$flight$Strata=="Low"])


  selected.data$obs$Species[selected.data$obs$Species == "CCGO" & selected.data$obs$ctran %in% Lows]="TAVS"


    #full.data$Species[full.data$Stratum=="Low" & full.data$Species=="CCGO"]="TAVS"

  selected.data$obs$Species[selected.data$obs$Lat>=63 & selected.data$obs$Species=="CCGO"]="TAVS"

  return(selected.data)

}




StrataSummary=function(strata.file){


  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.file, type="proj")
  strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84")


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

 if("Egg Island" %in% strata.area$strata){

   strata.area$diff[strata.area$strata=="Egg Island"]=10
   strata.area$M[strata.area$strata=="Egg Island"]=50


 }

  return(strata.area)
}



TrimToStrata=function(full.data, strata.path){


  coordinates(full.data)=~Lon+Lat
  proj4string(full.data)=CRS("+proj=longlat +ellps=WGS84")

  strata.proj=LoadMap(strata.path, type="proj")
  strata.proj <- sp::spTransform(strata.proj, "+proj=longlat +ellps=WGS84")

  full.data=raster::intersect(full.data, strata.proj)

  return(as.data.frame(full.data))


}



EstimatesTable=function(area, year){

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

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    data=DataSelect(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path)
    est=Densities(data, area=entries$AREA[i])

    if(i==1){output.table=est$estimates
             expanded.table=est$counts.final}
    if(i>1){output.table=rbind(output.table, est$estimates)
            expanded.table=rbind(expanded.table, est$counts.final)}

    }

  output.table$area=area
  expanded.table$area=area


  combined=CombineEstimates(output.table)
  combined$area=area

  return(list(output.table=output.table, expanded.table=expanded.table, combined=combined))


}



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



SpeciesTransect=function(area, year, species){

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
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/YKG_1988_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }
      if(area=="CRD" & rep==1 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/YKG_1998_QCObs_SeatLF.csv", sep="")

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
        output.table$obs.area=data$transect$area[data$transect$ctran==output.table$ctran][1]
        output.table$strata=data$transect$strata[data$transect$ctran==output.table$ctran][1]

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
        temp.table$obs.area=data$transect$area[data$transect$ctran==temp.table$ctran][1]
        temp.table$strata=data$transect$strata[data$transect$ctran==temp.table$ctran][1]

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
  output.flkdrake$freq=0

  sum.flkdrake = aggregate(freq~Year+Observer+Species+Obs_Type+ctran+Num+obs.area+strata+strata.area+area, data=output.flkdrake, FUN = length)

  for(n in 1:length(sum.flkdrake$Year)){

    sum.flkdrake$obs.area[n]=output.table$obs.area[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]
    sum.flkdrake$strata[n]=output.table$strata[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]
    sum.flkdrake$strata.area[n]=output.table$strata.area[output.table$Year==sum.flkdrake$Year[n] & output.table$ctran==sum.flkdrake$ctran[n]][1]


  }

  output.table = output.table[,!names(output.table)%in%c("itotal", "total", "ibb", "sing1pair2", "flock")]

  output.table=rbind(output.table, sum.flkdrake)

  output.table=output.table[order(output.table$Year, output.table$Observer, output.table$Species, output.table$ctran, output.table$Obs_Type),]

  return(list(output.table, M.table, output.flkdrake))


}



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

