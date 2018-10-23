

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


SwanCheck=function(data){

  nest=data[data$sppn=="TUNE" | data$sppn=="TRNE",]
  swan=data[data$sppn=="TUSW" | data$sppn=="TRSW",]

  bad=nest[which(nest$grp != "open"),]
  missing=nest[which(nest$)]


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


