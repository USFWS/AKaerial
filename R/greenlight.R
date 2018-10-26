

GreenLight=function(path.name, area){

  necessary=c("year", "month", "day", "seat", "obs", "strata", "tran", "seg", "dir", "wind", "wspd", "sky", "wavfile", "lat", "long", "ctime", "lag", "species", "group", "unit", "code", "notes")


  type <- file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE)}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "BPOP")){

    print("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, BPOP.")
    break
  }


  #check for missing or outlier lat/longs
  latlong=SpatialNA(dat1,method="greenlight")


  if(is.na(latlong)){
  n.latlong=0
  s.latlong="green"
  }
  else {
  n.latlong=length(latlong[,1])
  s.latlong="yellow"
  }


  #are the columns there that need to be renamed?
  test.switchmatch=SwitchMatch(data)

  if(test.switchmatch==TRUE){
    s.switchmatch="green"
  }
  else{s.switchmatch="red"}



  #are all of the required columns there (and named correctly)?
  test.colmatch=ColMatch(data, necessary=necessary)

  if(test.colmatch==TRUE){s.colmatch="green"}
  else{s.colmatch="red"}


  #are there any swan nests without associated records (missing) or swan nests recorded as
  #anything but open?

  test.swan=SwanCheck(data)

  if(test.swan$fail==TRUE){s.swan="red"}
  else{s.swan="green"}


  #are there only 4 possible unit types?

  test.unit=UnitCheck(data)

  if(test.unit$fail==TRUE){s.unit="red"}
  else{s.unit="green"}


  #are there only 4 possible seats recorded?

  test.seat=SeatCheck(data)

  if(test.seat$fail==TRUE){s.seat="red"}
  else{s.seat="green"}

  #are the species codes correct?

  test.species=SpeciesCheck(data)

  if(length(test.species$change)>0){s.species="yellow"}
  if(length(test.species$bad)>0){s.species="red"}
  if(test.species$fail==FALSE){s.species="green"}



  rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

  render(rmd.path, output_dir=getwd())

}



SwitchMatch=function(data){

  necessary=c("yr", "se", "mo", "strat", "da", "grp", "sppn")
  return(all(necessary %in% colnames(data)))

}


ColSwitch=function(data){

  colnames(data)[which(colnames(data)=="yr")]="year"
  colnames(data)[which(colnames(data)=="mo")]="month"
  colnames(data)[which(colnames(data)=="da")]="day"
  colnames(data)[which(colnames(data)=="se")]="seat"
  colnames(data)[which(colnames(data)=="strat")]="strata"
  colnames(data)[which(colnames(data)=="grp")]="group"
  colnames(data)[which(colnames(data)=="sppn")]="species"

  return(data)
}


ColMatch=function(data, necessary){

  return(all(necessary %in% colnames(data)))

}


ShouldBeNumeric=function(col.data){

bad=which(is.na(as.numeric(col.data)))


return(list("fail"=any(bad),"bad"=bad))

}


UnitCheck=function(data){

units=c("single", "pair", "flkdrake", "open")

bad=data[which(!(data$unit %in% units)),]

return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}

SeatCheck=function(data){

  seats=c("lf", "rf", "lr", "rr")


  if("se" %in% colnames(data)){
  bad=data[which(!(data$se %in% seats)),]
  }

  if("seat" %in% colnames(data)){
    bad=data[which(!(data$seat %in% seats)),]
    }

  return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}


SpeciesCheck=function(data){



  if("sppn" %in% colnames(data)){
    bad=data[which(!(data$sppn %in% sppntable$should.be) & !(data$sppn %in% sppntable$sppn)),]

    change=data[which(data$sppn %in% sppntable$sppn & (!(data$sppn %in% sppntable$should.be))),]

  }

  if("species" %in% colnames(data)){
    bad=data[which(!(data$species %in% sppntable$should.be) & !(data$species %in% sppntable$sppn)),]

    change=data[which(data$species %in% sppntable$sppn & (!(data$species %in% sppntable$should.be))),]
    }


  return(list("fail"=(length(bad[,1])!=0), "bad"=bad, "change"=change))



}


SwanCheck=function(data){

  nest=data[data$sppn=="TUNE" | data$sppn=="TRNE",]
  swan=data[data$sppn=="TUSW" | data$sppn=="TRSW",]



  bad=nest[which(nest$unit != "open"),]
  missing=nest[which(!(nest$lat %in% swan$lat & nest$long %in% swan$long)),]

  return(list("fail"=(length(bad[,1])!=0 | length(missing[,1])!=0), "bad"=bad, "missing"=missing))

}






ObsByYear=function(path.name){

  type <- file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE)}

  yr.list=unique(data$yr)

  data$obs=toupper(data$obs)

  for(year in seq_along(yr.list)){

    temp.data=data[data$yr==yr.list[year],]
    obs.list=unique(temp.data$obs)

    print(obs.list)
    print(length(obs.list))

    for (i in 1:length(obs.list)){
    write.csv(temp.data[temp.data$obs==obs.list[i],],
              file=paste("Q:/Waterfowl/Parsed/ACP_", yr.list[year], "_RawObs_", obs.list[i], ".csv", sep=""),
              row.names=FALSE,
              col.names=names(temp.data),
              quote=FALSE
              )
    }

  }


}


