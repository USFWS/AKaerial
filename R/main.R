
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

DataSelect <- function(area, year, path=NA, data=NA, observer="all", seat="all", strata="all", species="all", method="other", zeroes=FALSE, endpts=TRUE, show.data=TRUE){



  #if no R object is passed as data
  if(length(data)<1){



  if(!is.na(path)){year=1}

  if(is.na(path)){
  if(area=="ykd"){path = system.file("external/YKD8516dat.csv", package="AKaerial")}
  else{
    if (area=="ykg"){path = system.file("external/YKG8516dat.csv", package="AKaerial")}
      else{
        if (area=="acp"){path = system.file("external/ACP9717dat.csv", package="AKaerial")}
          else{
    print("Area not specified or incorrect.")
    break
          }}}
  }


  data=read.csv(path, header=TRUE)

  }

  else{year=1}



  if(year==1){year=c(as.numeric(unique(data$yr[which.min(data$yr)])),as.numeric(unique(data$yr[which.max(data$yr)]))) }

  #print("check year")
  if (length(year)==1){year= rep(year, 2)}

  data$yr=zoo::na.approx(data$yr)
  data=data[data$yr>=year[1] & data$yr<=year[2],]

  #print("SpatialNA")
  data=SpatialNA(data)


  #print("PointsToStrata")
  data=PointsToStrata(data,area)


  #print("Filters")
  if(strata != "all"){data=data[data$strat %in% strata,]}

  data$obs=sapply(data$obs, toupper)

  data$obs[data$obs=="WL"]="WWL"
  data$obs[data$obs=="RM"]="RDM"

  if(observer != "all"){data=data[data$obs==observer,]}

  data$se=sapply(data$se, toupper)

  if(seat != "all"){data=data[data$se %in% seat,]}

  data$sppn=sapply(data$sppn, toupper)

  data$sppn=ShouldBe(data$sppn)

  data$grp=as.numeric(as.character(data$grp))

  if(species != "all"){

    species2=c(species, "START", "ENDPT")
    data=data[data$sppn %in% species2,]

  }

  #print("CorrectTrans")
  data=CorrectTrans(data, area=area)

  #print("CorrectUnit")
  data=CorrectUnit(data)

  data=droplevels(data)

  if(species != "all"){

  obs.data=data[data$sppn %in% species,]

  }

  #print(unique(data$ctran))
  #print("TransSummary")
  flight=TransSummary(data, area)
  flight=flight[flight$len>0,]

  data$sppn=as.character(data$sppn)
  data=TrimData(data, area)



  show.plot=ViewStrata(area=area, year=year[1], ViewTrans=TRUE, numbers=TRUE, print=FALSE)
  show.points=show.plot+geom_point(data=data, aes(x=long, y=lat, shape=unit)) + theme(legend.position="top") + scale_shape_discrete(name="Unit", solid=F)
  if(show.data==TRUE){print(show.points)}

  if(zeroes==TRUE){data=MakeZeroes(data)}

  if(endpts==FALSE){data=data[data$sppn != "START" & data$sppn != "ENDPT", ]}


  data=AdjustCounts(data)

  data=list("obs"=data, "flight"=flight)

  if(method=="transect"){data=TransData(data)}


  return(data)

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

  #groupings list
  unit.list=c("single", "pair","open", "flkdrake")
  #list of years
  yr.list=as.character(unique(selected.data$flight$yr))
  #list of species
  sp.list=as.character(unique(selected.data$obs$sppn))

  if("closest" %in% names(selected.data$obs)){selected.data$obs=selected.data$obs[,!(names(selected.data$obs) %in% "closest")]}
  if("dist" %in% names(selected.data$obs)){selected.data$obs=selected.data$obs[,!(names(selected.data$obs) %in% "dist")]}


  #grid method

  for (observer in unique(selected.data$flight$obs)){

    #print(observer)

    yr.list=unique(selected.data$flight$yr[selected.data$flight$obs==observer])

    for (year in yr.list){

      tran.list=unique(selected.data$flight$part.of[selected.data$flight$yr==year & selected.data$flight$obs==observer])

      new.rows=expand.grid(year, NA, NA, NA, observer, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list, 0, unit.list, NA, tran.list,0,0,0,0)

      #print(length(new.rows))

      #print(names(selected.data$obs))

      names(new.rows)=names(selected.data$obs)
      selected.data$obs=rbind(selected.data$obs,new.rows)
      #print(year)
    }
  }



  #cycle through, check each transect/observer combo for each species
  #for (h in 1:length(yr.list)){
  #  sub.data=selected.data$obs[selected.data$obs$yr==yr.list[h],]

  #  obs.list=unique(as.character(selected.data$flight$obs[selected.data$flight$yr==yr.list[h]]))

  #  print(paste("Making zeroes for year ", yr.list[h]))
  #  for (i in 1:length(sp.list)){

  #    sub.data=sub.data[sub.data$sppn==sp.list[i],]

  #   for (j in 1:length(obs.list)){
  #      print(paste("Observer ", obs.list[j]))
  #      tran.list=unique(selected.data$flight$part.of[selected.data$flight$yr==yr.list[h] & selected.data$flight$obs==obs.list[j]])

  #      sub.data=sub.data[as.character(sub.data$obs)==obs.list[j],]

  #      for (k in 1:length(tran.list)){

  #        sub.data=sub.data[as.character(sub.data$ctran)==tran.list[k],]

  #        for (m in 1:length(unit.list)){

          #skip if count exists


            #if(any(as.character(selected.data$obs$sppn)==sp.list[i] & as.character(selected.data$obs$ctran)==tran.list[k] & as.character(selected.data$obs$obs)==obs.list[j] & selected.data$obs$yr==yr.list[h] & selected.data$obs$unit==unit.list[m]))
  #          if(any(sub.data$unit==unit.list[m]))
  #            {next}

            #add the 0 row
  #          new.row=c(yr.list[h], NA, NA, NA, obs.list[j], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list[i], 0, unit.list[m], NA, NA, tran.list[k], NA, 0)
  #          selected.data$obs=rbind(selected.data$obs,new.row)



  #        } #end unit

  #      } #end tran

  #    } #end obs
  #  } #end sp
  #} #end yr

  selected.data$obs$grp=as.numeric(selected.data$obs$grp)


  agg=aggregate(cbind(grp,itotal,total,ibb, sing1pair2)~yr+obs+sppn+unit+ctran,data=selected.data$obs, FUN=sum)


  colnames(agg)=c("yr", "obs", "sppn", "unit", "ctran", "grp", "itotal", "total", "ibb", "sing1pair2")


  agg$area=0

  agg$strata="none"


  for(g in 1:length(agg$area)){

    agg$area[g]=sum(selected.data$flight$sampled.area[selected.data$flight$yr==agg$yr[g] & selected.data$flight$obs==agg$obs[g] & selected.data$flight$part.of==agg$ctran[g]])
    agg$strata[g]=selected.data$flight$strata[selected.data$flight$yr==agg$yr[g] & selected.data$flight$obs==agg$obs[g] & selected.data$flight$part.of==agg$ctran[g]][1]

  }




return(agg[order(agg$yr, agg$obs, agg$sppn, as.numeric(agg$ctran), agg$unit),])

}


SplitDesign <- function(area, SegCheck=FALSE, file.name, layer.name){

  #design=readOGR("D:/CharlesFrost/AKaerial/data/TransectDesign/YKD_trans_B.shp", "YKD_trans_B")
  #file.name = system.file("external/YKD_trans_B.shp", package="AKaerial")

  design=readOGR(file.name, layer.name, verbose=FALSE)
  design.proj <- spTransform(design, "+proj=longlat +ellps=WGS84")

  strata.proj=LoadMap(area, type="proj")

  newlines = raster::intersect(design.proj, strata.proj)
  newlines@data$id=rownames(newlines@data)
  newlines.fort=fortify(newlines, region="STRAT")
  newlines.df=join(newlines.fort, newlines@data, by="id")

  if(area=="ykg"){
  newlines.df=newlines.df[,c("long","lat", "order", "piece", "id", "OBJECTID", "STRAT")]
}
  if(area=="ykd"){
  newlines.df=newlines.df[,c("long","lat", "order", "piece", "id", "OBJECTID", "STRAT")]
}

  #newlines.df$order[newlines.df$order==3]=1
  #newlines.df$order[newlines.df$order==4]=2
  #newlines.df$order[newlines.df$order==5]=1
  #newlines.df$order[newlines.df$order==6]=2

  newlines.df$order[(newlines.df$order %% 2)==1]=1
  newlines.df$order[(newlines.df$order %% 2)==0]=2


  if(SegCheck==TRUE){

    temp=aggregate(newlines.df$order~newlines.df$OBJECTID+newlines.df$STRAT, FUN="length")
    colnames(temp)=c("original", "strata", "segs")
    temp$segs=temp$segs/2
    temp=temp[order(temp$original, temp$strata),]
    write.table(temp, file="segcheck.txt", quote=FALSE, row.names=FALSE)


  }


  colnames(newlines.df)[6]="original"

  newlines.df$id=rep(1:(length(newlines.df$id)/2), each=2)


  return(newlines.df)



}





LoadMap <- function(area, type="df") {

  if(area=="acp"){

  map = system.file("external/ACP_2018_AnalysisStrata.shp", package="AKaerial")
  #map="D:/CharlesFrost/AKaerial/data/a483web7 polygon.shp"
  lay="ACP_2018_AnalysisStrata"
  }

  if(area=="ykd"){
  map = system.file("external/newaird3_polygon.shp", package="AKaerial")
  #map="D:/CharlesFrost/AKaerial/data/newaird3_polygon.shp"
  lay="newaird3_polygon"
  }

  if(area=="ykg"){
  map = system.file("external/YKG__2018_MemoAnalysisStrata.shp", package="AKaerial")
  #map="D:/CharlesFrost/AKaerial/data/StratificationForHodgesAnalysis2.shp"
  lay="YKG__2018_MemoAnalysisStrata"

  }

  if(area=="crd"){
    map = system.file("external/CRD_2018_AnalysisStrata.shp", package="AKaerial")
    #map="D:/CharlesFrost/AKaerial/data/CRD_2018_AnalysisStrata.shp"
    lay="CRD_2018_AnalysisStrata"

  }


  maptools::gpclibPermit()
  strata <- readOGR(map, lay, verbose=FALSE)

  strata.proj <- spTransform(strata, "+proj=longlat +ellps=WGS84")
  strata<- gBuffer(strata, byid=TRUE, width=0)



  strata.proj@data$id = rownames(strata.proj@data)

  #ifelse(area=="acp", strata.fort <- fortify(strata.proj, region="STRATNAME"), strata.fort <- fortify(strata.proj, region="STRAT"))
  strata.fort <- fortify(strata.proj, region="id")

  strata.df=join(strata.fort, strata.proj@data, by="id")

  if(type=="df") {return(strata.df)}
  if(type=="proj") {return(strata.proj)}
}


ViewStrata <- function(area, year=NULL, ViewTrans=FALSE, strata="all", numbers=FALSE, print=TRUE) {
  GIS.obj = LoadMap(area)
if(strata=="all"){

  if(area=="acp" || area=="crd"){
    strata.plot <- ggplot() +
      geom_path(data=GIS.obj, aes(long,lat,group=group)  ) +
      geom_path(color="black") +
      coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))
    #scale_fill_manual(name="Strata", values=c("red","green","yellow","grey", "orange"))
  }

  if(area=="ykd"){
    strata.plot <- ggplot() +
      geom_polygon(data=GIS.obj, aes(long,lat,group=group,fill=id)  ) +
      geom_path(color="black") + coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))
  }

  if(area=="ykg"){
    strata.plot <- ggplot() +
      geom_polygon(data=GIS.obj, aes(long,lat,group=group,fill=id)  ) +
      geom_path(color="black") + coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))
  }

}

  if(strata!="all"){

    data=GIS.obj[as.character(GIS.obj$STRATNAME) %in% strata,]


    if(area=="acp" || area=="crd"){
      strata.plot <- ggplot() +
        geom_path(data=data, aes(long,lat,group=group)  ) +
        geom_path(color="black", lwd=1.5) +
        coord_map(xlim=c(min(data$long), max(data$long)), ylim=c(min(data$lat), max(data$lat))) +
        scale_x_continuous("Longitude (degrees)") + scale_y_continuous("Latitude (degrees)")
      }



    if(area=="ykd"){
      strata.plot <- ggplot() +
        geom_polygon(data=GIS.obj, aes(long,lat,group=group,fill=id)  ) +
        geom_path(color="black") + coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))
    }

    if(area=="ykg"){
      strata.plot <- ggplot() +
        geom_polygon(data=GIS.obj, aes(long,lat,group=group,fill=id)  ) +
        geom_path(color="black") + coord_map(xlim=c(min(GIS.obj$long), max(GIS.obj$long)), ylim=c(min(GIS.obj$lat), max(GIS.obj$lat)))
    }

  }



  if(ViewTrans){
    trans=TranSelect(year=year, area=area)
    trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
    trans.layer=trans$layer

    trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

    GIS.proj = LoadMap(area, type="proj")

    if(area=="crd"){trans.proj=raster::intersect(trans.proj, GIS.proj)}  #trim the excess lines


    trans.proj@data$id = rownames(trans.proj@data)

    trans.df=fortify(trans.proj, region=OBJECTID)

    trans.df=join(trans.df,trans.proj@data, by="id")

    trans.labels=trans.df[trans.df$order==1,]





    strata.plot = strata.plot +
      geom_path(data=trans.df, aes(x=long, y=lat, group=group))

    if(numbers==TRUE){
    strata.plot = strata.plot +
      geom_text(data=trans.labels, aes(x=long, y=lat, label=OBJECTID), size=3, fontface="bold")
    }



  }

  if(print==TRUE){print(strata.plot)}
  return(strata.plot)
}






AdjustCounts <- function(full.data){


  full.data$grp=as.numeric(as.character(full.data$grp))

  for (i in 1:length(full.data$grp)){

    #0 out the coded start and end points (codes are logged in the species column)
    if(full.data$sppn[i]=="START" | full.data$sppn[i]=="ENDPT" | full.data$sppn[i]=="start" | full.data$sppn[i]=="end" | full.data$sppn[i]=="ENDT" | full.data$sppn[i]=="BEGT") {
      full.data$itotal[i]=0
      full.data$total[i]=0
      full.data$ibb[i]=0
      full.data$sing1pair2[i]=0
    }

    #Double singles for indicated totals
    else if(full.data$unit[i]=="single") {
      full.data$itotal[i]=2*full.data$grp[i]
      full.data$ibb[i]=2*full.data$grp[i]
      full.data$total[i]=full.data$grp[i]
      full.data$sing1pair2[i]=full.data$grp[i]
    }

    #Pairs are doubled for both total and indicated total
    else if(full.data$unit[i]=="pair") {
      full.data$itotal[i]=2*full.data$grp[i]
      full.data$ibb[i]=2*full.data$grp[i]
      full.data$total[i]=2*full.data$grp[i]
      full.data$sing1pair2[i]=2*full.data$grp[i]
    }

    #Open indicates a flock, nothing doubled, zero for ibb
    else if(full.data$unit[i]=="open"){
      full.data$itotal[i]=full.data$grp[i]
      full.data$total[i]=full.data$grp[i]
      full.data$ibb[i]=0
      full.data$sing1pair2[i]=0

    }

    #Flocked drakes are doubled for 1-4 seen for indicated bb/totals.  Reference would be useful.
    else if(full.data$unit[i]=="flkdrake" & full.data$grp[i]<5){
      full.data$itotal[i]=2*full.data$grp[i]
      full.data$total[i]=full.data$grp[i]
      full.data$ibb[i]=2*full.data$grp[i]
      full.data$sing1pair2[i]=0

    }

    #Flocked drakes 5 and above aren't doubled because of science stuff, 0 for ibb.
    else if(full.data$unit[i]=="flkdrake" & full.data$grp[i]>4){
      full.data$itotal[i]=full.data$grp[i]
      full.data$total[i]=full.data$grp[i]
      full.data$ibb[i]=0
      full.data$sing1pair2[i]=0

    }


  }


sppn=unique(full.data$sppn)

#  for (i in 1:length(sppn)){

#      itot=sppntable$itot[as.character(sppntable$sppn)==as.character(sppn[i])]
#      full.data$itotal[full.data$sppn==as.character(sppn[i])]=itot*full.data$itotal[full.data$sppn==as.character(sppn[i])]

#    }



  return(full.data)
}



CountsTable=function(adj.counts) {



    t1=(aggregate(adj.counts$total~adj.counts$yr+adj.counts$obs+adj.counts$ctran+adj.counts$sppn+adj.counts$strata, FUN=sum))
    t2=(aggregate(adj.counts$itotal~adj.counts$yr+adj.counts$obs+adj.counts$ctran+adj.counts$sppn+adj.counts$strata, FUN=sum))
    t2b=aggregate(adj.counts$ibb~adj.counts$yr+adj.counts$obs+adj.counts$ctran+adj.counts$sppn+adj.counts$strata, FUN=sum)
    t4=aggregate(adj.counts$sing1pair2~adj.counts$yr+adj.counts$obs+adj.counts$ctran+adj.counts$sppn+adj.counts$strata, FUN=sum)
    t3=merge(t1,t2,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$ctran", "adj.counts$sppn", "adj.counts$strata"))
    t3=merge(t3,t2b,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$ctran", "adj.counts$sppn", "adj.counts$strata"))
    t3=merge(t3,t4,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$ctran", "adj.counts$sppn", "adj.counts$strata"))


    colnames(t3)=c("yr","obs","ctran", "sppn", "strata", "total", "itotal", "ibb", "sing1pair2")

    return(t3[order(t3$yr, t3$obs, t3$sppn, as.numeric(t3$ctran)),])




} #end CountsTable

PointsToStrata=function(full.data, area){


  #full.data=SpatialNA(full.data)

  x=na.approx(full.data$long, na.rm=FALSE)
  y=na.approx(full.data$lat, na.rm=FALSE)


  sp=cbind(x,y)

  sp=SpatialPoints(sp)

  proj4string(sp)=CRS("+proj=longlat +ellps=WGS84")

  sp=spTransform(sp, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  map=LoadMap(area, type="proj")

  map=spTransform(map, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0
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


  #area=toupper(area)

  #year=as.numeric(year)

  #if((year %% 4)==1){letter="B"}
  #if((year %% 4)==2){letter="C"}
  #if((year %% 4)==3){letter="D"}
  #if((year %% 4)==0){letter="A"}

  #trans.layer=paste(area, "trans", letter, sep="_")
  #trans.file=paste(trans.layer,".shp", sep="")

  #trans=list("file"=trans.file, "layer"=trans.layer)

  return(trans)
}







Densities=function(data, n.obs=1, trans.method="gis", trans.width=.2, area, output=TRUE) {

  #data=read.csv(file=(system.file("external/YKG17_HMW_MAS.v7.5.17.csv", package="AKaerial")), header=TRUE)
  #trans=TranSelect(year=data$yr[1], area=area)

  #trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
  #trans.layer=trans$layer
  shp=LoadMap(area=area, type="proj")

  #Save old warning settings to revert to before leaving function
  oldw <- getOption("warn")
  #Suppress spatial warnings inside function call
  options(warn=-1)

  #Attach strata ownership to data points (moved to DataSelect)
  #data=PointsToStrata(data,area=area)
  #Change col type from factor to character
  #data$strat=as.character(data$strat)

  #Compile the length (d), strata (type), area covered (des.area), and original trans name in data (original)
  #Splits transects by strata if needed
  #transects=TransectTable(trans.file=trans.file, trans.layer=trans.layer, method=trans.method, area=area, obs=n.obs)
  #transects=transects[,-1]

  #Moved NA function to DataSelect
  #Find any NA for strata ownership (caused by late/early click on receiver in plane)
  #for(a in 1:length(data$strat)){

    #If NA found, replace with strata type shared by entries on that transect
    #This assumes the click/logging was actually over the strata and not ACTUALLY early/late
  #  if(is.na(data$strat[a])){
  #    temp=data[data$tran==data$tran[a],]
  #    data$strat[a]=names(sort(table(temp$strat), decreasing=TRUE))[1]

  #  }

  #}

  #Compute the total/indicated total for the group sizes indicated in the data
  #adj.counts=AdjustCounts(data)

  #Sum the counts by combinations of species/transect
  counts.t=CountsTable(data)
  counts.t$area=0

  for (i in 1:length(counts.t$area)){

    counts.t$area[i]=data$area[data$yr==counts.t$yr[i] & data$obs==counts.t$obs[i] & data$ctran==counts.t$ctran[i]][1]

  }

  #Add a row with a 0 count for every species counted somewhere in the data but not on a given transect
  #t3=MakeZeroes(data, counts.t)
   t3=counts.t
  #t4=merge(t3, transects, by.x=c("tran", "strat"), by.y=c("original", "type"))

  #Remove transect start and end from species list
  t3=t3[t3$sppn != "ENDPT",]
  t3=t3[t3$sppn != "START",]
  #t3=t3[t3$sppn != "end",]
  #t3=t3[t3$sppn != "start",]
  #t3=t3[t3$sppn != "ENDT",]
  #t3=t3[t3$sppn != "BEGT",]

  #Make sure totals are numeric
  t3$total=as.numeric(t3$total)
  t3$itotal=as.numeric(t3$itotal)
  t3$ibb=as.numeric(t3$ibb)
  t3$sing1pair2=as.numeric(t3$sing1pair2)

  #t3$des.area=0


  #Sum the sampled areas for segments of the same transect
  #trans.area=aggregate(des.area~type+original, transects, sum)
  #des.area.sum=aggregate(des.area~type, transects,sum)


  #for (a in 1:length(t3$sppn)){

  #  if(any(trans.area$des.area[trans.area$original==t3$tran[a] & trans.area$type==t3$strat[a]])){
  #    t3$des.area[a]=trans.area$des.area[trans.area$original==t3$tran[a] & trans.area$type==t3$strat[a]]
  #  }


  #}

  #Trim off nonsense pieces created by gis clipping
  #t3=t3[t3$des.area>.5,]

  #Sum the counts of each species by strata type
  sp.strat.total=aggregate(t3$total~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.total)=c("yr","obs", "sppn", "strata", "total")

  sp.strat.itotal=aggregate(t3$itotal~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.itotal)=c("yr","obs", "sppn", "strata", "itotal")

  sp.strat.ibb=aggregate(t3$ibb~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.ibb)=c("yr","obs", "sppn", "strata", "ibb")

  sp.strat.sing1pair2=aggregate(t3$sing1pair2~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.sing1pair2)=c("yr","obs", "sppn", "strata", "sing1pair2")

  #Variance of the counts within each strata
  sp.strat.total.v=aggregate(t3$total~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.total.v)=c("yr", "obs", "sppn", "strata", "total.v")

  sp.strat.itotal.v=aggregate(t3$itotal~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.itotal.v)=c("yr", "obs", "sppn", "strata", "itotal.v")

  sp.strat.ibb.v=aggregate(t3$ibb~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.ibb.v)=c("yr","obs", "sppn", "strata", "ibb.v")

  sp.strat.sing1pair2.v=aggregate(t3$sing1pair2~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.sing1pair2.v)=c("yr","obs", "sppn", "strata", "sing1pair2.v")

  sp.strat=merge(sp.strat.total, sp.strat.itotal)
  sp.strat=merge(sp.strat, sp.strat.ibb)
  sp.strat=merge(sp.strat, sp.strat.sing1pair2)

  sp.strat.v=merge(sp.strat.total.v, sp.strat.itotal.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.ibb.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.sing1pair2.v)

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

  #Calculate covariance of total counts and area sampled

  for (i in 1:length(sp.strat.final$strata)){

    temp.t3=t3[t3$yr==sp.strat.final$yr[i] & t3$obs==sp.strat.final$obs[i] & t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i],]
    sp.strat.final$total.cov[i]=cov(temp.t3$total, temp.t3$area)
    sp.strat.final$itotal.cov[i]=cov(temp.t3$itotal, temp.t3$area)
    sp.strat.final$ibb.cov[i]=cov(temp.t3$ibb, temp.t3$area)
    sp.strat.final$sing1pair2.cov[i]=cov(temp.t3$sing1pair2, temp.t3$area)

  }


  #Calculate the total area by type and the variance of the areas

  area.strat=aggregate(t3$area~t3$yr+t3$obs+t3$strata+t3$sppn, FUN=sum)
  area.strat.v=aggregate(t3$area~t3$yr+t3$obs+t3$strata+t3$sppn, FUN=var)

  colnames(area.strat)=c("yr", "obs", "strata", "sppn","total.area")
  colnames(area.strat.v)=c("yr", "obs", "strata", "sppn", "total.area.var")

  area.strat=area.strat[!duplicated(area.strat[1:3]),-4]
  area.strat.v=area.strat.v[!duplicated(area.strat.v[1:3]),-4]


  #Put spatial summary together
  area.summary=merge(area.strat, area.strat.v)
  #print(area.summary)

  #Merge the counts and spatial stats
  counts.final=merge(sp.strat.final,area.summary, by=c("yr", "obs", "strata"))

  #Calculate final densities for each strata layer
  density.total=counts.final$total/counts.final$total.area
  density.itotal=counts.final$itotal/counts.final$total.area
  density.ibb=counts.final$ibb/counts.final$total.area
  density.sing1pair2=counts.final$sing1pair2/counts.final$total.area


  counts.final=cbind(counts.final, density.total, density.itotal, density.ibb, density.sing1pair2)
  #print(head(counts.final))

  #Get actual areas from gis layers
  strata.area=aggregate(shp@data$AREA~shp@data$STRAT, FUN=sum)
  colnames(strata.area)=c("strata", "layer.area")

  #Convert from m^2 to km^2
  strata.area$layer.area=strata.area$layer.area / 1000000

  #print(strata.area)

  counts.final=merge(counts.final, strata.area, by="strata")

  #Extrapolate density estimates across area calculation
  total.est=counts.final$density.total * counts.final$layer.area
  itotal.est=counts.final$density.itotal * counts.final$layer.area
  ibbtotal.est=counts.final$density.ibb * counts.final$layer.area
  sing1pair2.est=counts.final$density.sing1pair2 * counts.final$layer.area

  counts.final=cbind(counts.final, total.est, itotal.est,ibbtotal.est, sing1pair2.est)


  #Summarize in table
  estimates=aggregate(counts.final$total.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates)=c("yr", "obs", "sppn", "total.est")

  estimates.i=aggregate(counts.final$itotal.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.i)=c("yr", "obs", "sppn","itotal.est")

  estimates.ibb=aggregate(counts.final$ibbtotal.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.ibb)=c("yr", "obs", "sppn","ibbtotal.est")

  estimates.sing1pair2=aggregate(counts.final$sing1pair2.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.sing1pair2)=c("yr", "obs", "sppn","sing1pair2.est")

  estimates=merge(estimates, estimates.i, by=c("yr", "obs", "sppn"))
  estimates=merge(estimates, estimates.ibb, by=c("yr", "obs", "sppn"))
  estimates=merge(estimates, estimates.sing1pair2, by=c("yr", "obs", "sppn"))


  #adj.counts=merge(adj.counts, transects, by.x="tran", by.y="original")

  ### Var(N) ###

  #Keep projection consistent
  shp.proj <- spTransform(shp, "+proj=longlat +ellps=WGS84")

  shp.proj@data$id = rownames(shp.proj@data)
  shp.fort <- fortify(shp.proj, region="STRAT")
  shp.df=join(shp.fort, shp.proj@data, by="id")

  ##extract min and max lat from shp.df, calc gcd and / by sampled width
  min.lat=aggregate(shp.df$lat~shp.df$id, FUN=min)
  max.lat=aggregate(shp.df$lat~shp.df$id, FUN=max)
  piece.min.lat=aggregate(shp.df$lat~shp.df$piece+shp.df$id, FUN=min)
  colnames(piece.min.lat)=c("piece", "id", "min")
  piece.max.lat=aggregate(shp.df$lat~shp.df$piece+shp.df$id, FUN=max)
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
  diff.lat$M=diff.lat$diff/(trans.width*n.obs)

  #print(transects)
  #Number of transects sampled (m of a possible M)
  #reps=aggregate(transects$original~transects$type, FUN=length)

  #reps=data.frame("strata"=transects$type, "original"=transects$original)
  #reps=unique(reps)

  #print(reps)
  reps2=aggregate(t3$ctran~t3$yr+t3$obs+t3$strata+t3$sppn, FUN=length)
  colnames(reps2)=c("yr", "obs", "strata", "sppn","m")

  reps2=reps2[!duplicated(reps2[1:3]),-4]


  diff.lat=merge(diff.lat, reps2, by="strata")

  diff.lat=merge(diff.lat, area.summary, by=c("yr", "obs", "strata"))

  #replace the Egg Island N/S facing transect M calculation
  if(area=="crd"){diff.lat$M[diff.lat$strata=="Egg Island"]=10/(trans.width*n.obs)}

  #print(diff.lat)

  #print(diff.lat)
  #See equation 12.9, p. 249 in "Analysis and Management of Animal Populations"
  #Williams, Nichols, Conroy; 2002

  for (j in 1:length(counts.final$sppn)){

    M=diff.lat$M[diff.lat$yr==counts.final$yr[j] & diff.lat$obs==counts.final$obs[j] & diff.lat$strata==counts.final$strata[j]]
    m=diff.lat$m[diff.lat$yr==counts.final$yr[j] & diff.lat$obs==counts.final$obs[j] & diff.lat$strata==counts.final$strata[j]]
    prop.m=((1-(m/M))/m)

    #if(counts.final$sppn[j]=="SPEI"){print((counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j])))}

    counts.final$var.N[j]=(M^2)*prop.m*(counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j]))
    counts.final$var.Ni[j]=(M^2)*prop.m*(counts.final$itotal.v[j]+(counts.final$density.itotal[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.itotal[j]*counts.final$itotal.cov[j]))
    counts.final$var.Nib[j]=(M^2)*prop.m*(counts.final$ibb.v[j]+(counts.final$density.ibb[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.ibb[j]*counts.final$ibb.cov[j]))
    counts.final$var.Nsing1pair2[j]=(M^2)*prop.m*(counts.final$sing1pair2.v[j]+(counts.final$density.sing1pair2[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.sing1pair2[j]*counts.final$sing1pair2.cov[j]))


  }


  var.est=aggregate(counts.final$var.N~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(var.est)=c("yr", "obs", "sppn","var.N")

  var.est.i=aggregate(counts.final$var.Ni~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(var.est.i)=c("yr", "obs", "sppn","var.Ni")

  var.est.ibb=aggregate(counts.final$var.Nib~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(var.est.ibb)=c("yr", "obs", "sppn","var.Nib")

  var.est.sing1pair2=aggregate(counts.final$var.Nsing1pair2~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(var.est.sing1pair2)=c("yr", "obs", "sppn","var.Nsing1pair2")

  estimates=merge(estimates, var.est, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.i, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.ibb, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.sing1pair2, by=c("yr", "obs", "sppn"), all=TRUE)


  estimates$SE=sqrt(estimates$var.N)
  estimates$SE.i=sqrt(estimates$var.Ni)
  estimates$SE.ibb=sqrt(estimates$var.Nib)
  estimates$SE.sing1pair2=sqrt(estimates$var.Nsing1pair2)


  estimates$total.est=as.integer(estimates$total.est)
  estimates$itotal.est=as.integer(estimates$itotal.est)
  estimates$ibbtotal.est=as.integer(estimates$ibbtotal.est)
  estimates$sing1pair2.est=as.integer(estimates$sing1pair2.est)


  options(warn = oldw)

  #Output tables to txt files if requested

  if(output==TRUE){
    write.table(counts.final, file="finalcounts.txt", quote=FALSE, row.names=FALSE)
    write.table(estimates, file="estimates.txt", quote=FALSE, row.names=FALSE)

  }

  return(list("estimates"=estimates, "diff.lat"=diff.lat, "strata.area"=strata.area, "counts.final"=counts.final))
}


CombineEstimates=function(estimates){

  yr.list=unique(estimates$yr)
  sp.list=unique(estimates$sppn)

  combined=data.frame(yr=rep(yr.list, each=length(unique(estimates$sppn))), sppn=rep(sp.list, length(yr.list)), total=0, total.var=0, total.se=0, itotal=0, itotal.var=0, itotal.se=0, ibb=0, ibb.var=0, ibb.se=0)

  for(i in 1:length(combined$yr)){


      combined$total[i]=mean(estimates$total.est[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])

      combined$itotal[i]=mean(estimates$itotal.est[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])

      combined$ibb[i]=mean(estimates$ibbtotal.est[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])

      combined$sing1pair2[i]=mean(estimates$sing1pair2.est[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])


       combined$total.var[i]=sum(estimates$var.N[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])/(length(estimates$var.N[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])^2)

      combined$itotal.var[i]=sum(estimates$var.Ni[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])/(length(estimates$var.Ni[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])^2)

      combined$ibb.var[i]=sum(estimates$var.Nib[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])/(length(estimates$var.Nib[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])^2)

      combined$sing1pair2.var[i]=sum(estimates$var.Nsing1pair2[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])/(length(estimates$var.Nsing1pair2[estimates$yr==combined$yr[i] & estimates$sppn==combined$sppn[i]])^2)


    }

  combined$total.se=sqrt(combined$total.var)
  combined$itotal.se=sqrt(combined$itotal.var)
  combined$ibb.se=sqrt(combined$ibb.var)
  combined$sing1pair2.se=sqrt(combined$sing1pair2.var)



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


    trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

    strata.proj=LoadMap(area, type="proj")

    newlines = raster::intersect(trans.proj, strata.proj)
    newlines@data$id=rownames(newlines@data)
    newlines.fort=fortify(newlines, region="STRAT")
    trans.points=join(newlines.fort, newlines@data, by="id")

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


  #if(method=="gis"){
  #  id=unique(id)

  #  for(i in seq(1,length(trans.points$lat),2)) {

  #    for(j in seq(1,length(trans.points$lat),2)){

  #     if(i != j){

  #      if(trans.points$STRAT[i]==trans.points$STRAT[j]){

  #          if(var(trans.points$lat[c(i,i+1,j,j+1)])*10000 < .2){
  #            trans.points$original[j]=trans.points$original[i]
  #            trans.points$original[j+1]=trans.points$original[i]

  #          }
  #        }
  #      }
  #    }}
  #  output=unique(merge(output, trans.points[,5:6], by="id"))
#}


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


CorrectTrans=function(full.data, area){

years=unique(full.data$yr)

coordinates(full.data)=~long+lat
proj4string(full.data)=CRS("+proj=longlat +ellps=WGS84")

if(area=="crd"){

  full.data$ctran=full.data$tran

}


if(area=="old acp"){

  full.data$ctran=full.data$tran
  full.data$closest=full.data$tran
  full.data$dist=0

  for (i in 1:length(years)){
    #print(years[i])

    trans=TranSelect(year=years[i], area=area)
    trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
    trans.layer=trans$layer


    trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")


    temp.data=full.data[full.data$yr==years[i],]

    for (j in seq_along(temp.data$closest)){
      temp.data$closest[j]=as.numeric(as.character(trans.proj$OBJECTID[which.min(suppressWarnings(gDistance(temp.data[j,],trans.proj,byid=TRUE)))]))
      temp.data$dist[j]=min(suppressWarnings(gDistance(temp.data[j,],trans.proj,byid=TRUE)))
       }

    full.data$closest[full.data$yr==years[i]]=temp.data$closest
    full.data$dist[full.data$yr==years[i]]=temp.data$dist


    #m=gDistance(full.data[full.data$yr==years[i],], trans.proj, byid=TRUE)
    #full.data$closest[full.data$yr==years[i]]=apply(m, 2, function(X) order(X)[1])
    #full.data$dist[full.data$yr==years[i]]=apply(m, 2, function(X) min(X)[1]) * 111


    trans.proj@data$id = rownames(trans.proj@data)
    trans.df=fortify(trans.proj, region=OBJECTID)

    trans.df=join(trans.df,trans.proj@data, by="id")

    trans.df$OBJECTID=as.numeric(as.character(trans.df$OBJECTID))

    old=unique(trans.df$ORIGID)
    new=array(NA,length(old))

    for (j in 1:length(old)){
      new[j]=trans.df$OBJECTID[trans.df$ORIGID==old[j]][1]

    }
    renum=data.frame(old=old, new=new)


    if(years[i]>2011){


    for (k in 1:length(full.data$ctran)){

      if(full.data$yr[k]==years[i]){

      full.data$ctran[k]=renum$new[renum$old==full.data$ctran[k]]

      }

    }

}


    if(years[i]<=2011){

      for (k in 1:length(full.data$ctran)){

        if(full.data$yr[k]==years[i]){

          full.data$ctran[k]=full.data$closest[k]

          #if(any(renum$old==full.data$closest[k])){

          #full.data$ctran[k]=renum$new[renum$old==full.data$closest[k]]

          }

          #else{full.data$ctran[k]==NA}

      }
  }

}

}  #end acp / ykd


if(area=="ykg" || area=="acp"){

  full.data$ctran=full.data$tran
  full.data$closest=full.data$tran
  full.data$dist=0

  for (i in 1:length(years)){
    #print(years[i])

    trans=TranSelect(year=years[i], area=area)
    trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
    trans.layer=trans$layer


    trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
    trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")


    temp.data=full.data[full.data$yr==years[i],]

    for (j in seq_along(temp.data$closest)){
      temp.data$closest[j]=as.numeric(as.character(trans.proj$OBJECTID[which.min(suppressWarnings(gDistance(temp.data[j,],trans.proj,byid=TRUE)))]))
      temp.data$dist[j]=min(suppressWarnings(gDistance(temp.data[j,],trans.proj,byid=TRUE)))
    }

    full.data$closest[full.data$yr==years[i]]=temp.data$closest
    full.data$dist[full.data$yr==years[i]]=temp.data$dist


    #m=gDistance(full.data[full.data$yr==years[i],], trans.proj, byid=TRUE)
    #full.data$closest[full.data$yr==years[i]]=apply(m, 2, function(X) order(X)[1])
    #full.data$dist[full.data$yr==years[i]]=apply(m, 2, function(X) min(X)[1]) * 111




    }

  full.data$ctran=full.data$closest

  }









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



TransSummary=function(full.data, area){

  observers=unique(as.character(full.data$obs))
  years=unique(full.data$yr)

  #print(observers)
  #print(years)

  #tsum=data.frame(yr=NULL,obs=NULL, orig=NULL, len=NULL, part.of=NULL)
  tsum=NULL

  for (i in 1:length(years)){


    if(area=="acp" || area=="ykg"){


      trans=TranSelect(year=years[i], area=area)
      trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
      trans.layer=trans$layer


      trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
      trans.obj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

      GIS.obj = LoadMap(area, type="proj")




    } #end acp/ykg

    if(area=="crd"){


      trans=TranSelect(year=years[i], area="crd")
      trans.file=system.file(paste("external/", trans$file, sep=""), package="AKaerial")
      trans.layer=trans$layer


      trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
      trans.obj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

      GIS.obj = LoadMap(area, type="proj")

      trans.obj=raster::intersect(trans.obj, GIS.obj)  #trim the excess lines


    } #end crd



    trans.obj@data$len=SpatialLinesLengths(trans.obj, longlat=TRUE)



    tpoints=as(trans.obj, "SpatialPointsDataFrame")
    tpoints=spTransform(tpoints, "+proj=longlat +ellps=WGS84")
    tpoints$strata=over(tpoints,GIS.obj)$STRATNAME



    # names(sort(table(tpoints$name[tpoints$OBJECTID==1]),decreasing=TRUE)[1])




    for (j in 1:length(observers)){


      if(area=="ykg"){


        if(length(full.data$long[full.data$obs==observers[j] & full.data$yr==years[i]])>0){


          obs.flown=full.data[!duplicated(full.data[c("yr","obs", "tran", "ctran")]),]

          obs.flown=obs.flown[obs.flown$yr==years[i] & obs.flown$obs==observers[j],]


          yr=obs.flown$yr
          obs=as.character(obs.flown$obs)
          renum=as.numeric(as.character(obs.flown$ctran))
          #print(renum)
          orig=as.numeric(as.character(obs.flown$tran))
          len=array(0,length(orig))
          strata=array(0,length(orig))


          for (k in 1:length(renum)){

            len[k]=sum(trans.obj@data$len[trans.obj@data$OBJECTID==renum[k]])


            strata[k]=names(which.max(table(full.data$strat[full.data$yr==years[i] & full.data$ctran==renum[k]])))


          } #end k


          temp.frame=data.frame(yr=yr, obs=obs, orig=orig, len=len, part.of=renum, strata=strata)

          #temp.frame=temp.frame[order(part.of),]

          tsum=rbind(tsum, temp.frame)

        } # end year loop

      } # end ykg loop





      if(area == "crd" || area=="acp"){


        if(length(full.data$long[full.data$obs==observers[j] & full.data$yr==years[i]])>0){

          obs.flown=full.data[!duplicated(full.data[c("yr","obs","tran","ctran")]),]

          obs.flown=obs.flown[obs.flown$yr==years[i] & obs.flown$obs==observers[j],]


          yr=obs.flown$yr
          obs=obs.flown$obs
          orig=as.numeric(as.character(obs.flown$tran))
          len=array(0,length(orig))
          strata=array(0,length(orig))
          part.of=as.numeric(as.character(obs.flown$ctran))

          for (k in 1:length(orig)){

            if (years[i]>2011){
              len[k]=sum(trans.obj@data$len[trans.obj@data$ORIGID==orig[k] & trans.obj@data$OBJECTID==part.of[k]])
            }

            if (years[i]<=2011){
              len[k]=sum(trans.obj@data$len[trans.obj@data$OBJECTID==part.of[k]])
            }

            #strata[k]=names(sort(table(tpoints$strata[tpoints$OBJECTID==part.of[k]]),decreasing=TRUE)[1])
            #strata[k]=names(which.max(table(tpoints$STRATNAME[tpoints$OBJECTID==part.of[k]])))
            strata[k]=names(which.max(table(full.data$strat[full.data$yr==years[i] & full.data$ctran==part.of[k]])))


          } #end k



          temp.frame=data.frame(yr=yr, obs=obs, orig=orig, len=len, part.of=part.of, strata=strata)

          temp.frame=temp.frame[order(orig),]

          if (years[i]<=2011 & area=="acp"){

            temp.frame=temp.frame[!duplicated(temp.frame[c("obs","len","part.of")]),]

          }

          tsum=rbind(tsum, temp.frame)


        } #end if any obs/yr

      } #end if not ykg

    } #end j observers

  } #end i years

  tsum$sampled.area=.2*tsum$len

  if(area=="ykg" || area=="crd"){tsum=tsum[!duplicated(tsum[,c("yr", "obs", "part.of")]),]}

  if(area=="acp"){

    tsum.agg=aggregate(list("len"=tsum$len,"sampled.area"=tsum$sampled.area), by=list("yr"=tsum$yr,"obs"=tsum$obs,"part.of"=tsum$part.of,"strata"=tsum$strata), FUN=sum)
    return(tsum.agg)

  }

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

    estimates$adjusted.ibb[estimates$sppn=="DCGO"]=estimates$ibb[estimates$sppn=="DCGO"]*3.3416
    estimates$adjusted.ibb.se[estimates$sppn=="DCGO"]=sqrt(((estimates$ibb.se[estimates$sppn=="DCGO"]/estimates$ibb[estimates$sppn=="DCGO"])^2)+((0.3244/3.3416)^2))*estimates$adjusted.ibb[estimates$sppn=="DCGO"]


  }


  return(estimates)
}



TrimData=function(full.data, area){

  if(area == "crd"){


    trimmed=subset(full.data, !is.na(full.data$strat))

    return(trimmed)

  }

  if(area == "ykg"){


    trimmed=subset(full.data, full.data$dist<.5)

    trimmed$sppn[trimmed$strat=="8" & trimmed$sppn=="CAGO"]="TAVS"
    trimmed$sppn[trimmed$lat>=63 & trimmed$sppn=="CAGO"]="TAVS"




    return(trimmed)

  }


  if(area=="acp"){


    trimmed=subset(full.data, full.data$dist<.5)

    return(trimmed)

  }




}


