

GreenLight=function(path.name, area){

  necessary=c("Year",
              "Month",
              "Day",
              "Seat",
              "Observer",
              "Stratum",
              "Transect",
              "Segment",
              "Flight_Dir",
              "A_G_Name",
              "Wind_Dir",
              "Wind_Vel",
              "Sky",
              "Filename",
              "Lat",
              "Lon",
              "Time",
              "Delay",
              "Species",
              "Num",
              "Obs_Type",
              "Behavior",
              "Distance",
              "Code",
              "Notes")


  type <- tools::file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE)}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "BPOP")){

    print("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, BPOP.")
    break
  }


  #check for missing or outlier lat/longs
  #took this out after meeting in 12/18; currently have no agreed on imputation scheme/treatment
  #latlong=SpatialNA(data,method="greenlight")


  # if(is.na(latlong[1])){
  # n.latlong=0
  # s.latlong="green"
  # }else {
  # n.latlong=length(latlong[,1])
  # s.latlong="yellow"
  # }


  #are the columns there that need to be renamed?
  test.switchmatch=SwitchMatch(data)

  if(test.switchmatch==TRUE){
    s.switchmatch="green"
  }
  else{s.switchmatch="red"}



  #are all of the required columns there (and named correctly)?
  test.colmatch=ColMatch(data, necessary=necessary)

  if(test.colmatch==TRUE){s.colmatch="green"}else{s.colmatch="red"}


  #are there any swan nests without associated records (missing) or swan nests recorded as
  #anything but open?

  test.swan=SwanCheck(data)

  if(test.swan$fail==TRUE){s.swan="red"}else{s.swan="green"}


  #are there only 4 possible unit types? (Obs_type = single, pair, open, flkdrake)

  test.unit=UnitCheck(data)

  if(test.unit$fail==TRUE){s.unit="red"}else{s.unit="green"}


  #are there only 4 possible seats recorded? (Seat = RF, RR, LF, LR)

  test.seat=SeatCheck(data)

  if(test.seat$fail==TRUE){s.seat="red"}else{s.seat="green"}

  #are the species codes correct?

  test.species=SpeciesCheck(data)

  if(length(test.species$change)>0){s.species="yellow"}
  if(length(test.species$bad)>0){s.species="red"}
  if(test.species$fail==FALSE){s.species="green"}


  #are the observer initials all uppercase?

  test.observer=ObserverCheck(data)

  if(test.observer==TRUE){s.observer="green"}else{s.observer="red"}


  #one observer per file?

  test.oneobserver=(length(unique(data$Observer))==1)

  if(test.oneobserver==TRUE){s.oneobserver="green"}else{s.oneobserver="red"}


  #Numeric tests- Year, Month, Day, Wind_Vel, Lat, Lon, Time, Delay, Num, Code

  test.year=ShouldBeNumeric(data$Year)
  if(test.year$fail==TRUE){s.year="red"}else{s.year="green"}

  test.month=ShouldBeNumeric(data$Month)
  if(test.month$fail==TRUE){s.month="red"}else{s.month="green"}

  test.day=ShouldBeNumeric(data$Day)
  if(test.day$fail==TRUE){s.day="red"}else{s.day="green"}

  test.wind=ShouldBeNumeric(data$Wind_Vel)
  if(test.wind$fail==TRUE){s.wind="red"}else{s.wind="green"}

  test.lat=ShouldBeNumeric(data$Lat)
  if(test.lat$fail==TRUE){s.lat="red"}else{s.lat="green"}

  test.lon=ShouldBeNumeric(data$Lon)
  if(test.lon$fail==TRUE){s.lon="red"}else{s.lon="green"}

  test.time=ShouldBeNumeric(data$Time)
  if(test.time$fail==TRUE){s.time="red"}else{s.time="green"}

  test.delay=ShouldBeNumeric(data$Delay)
  if(test.delay$fail==TRUE){s.delay="red"}else{s.delay="green"}

  test.num=ShouldBeNumeric(data$Num)
  if(test.num$fail==TRUE){s.num="red"}else{s.num="green"}

  test.code=ShouldBeNumeric(data$Code)
  if(test.code$fail==TRUE){s.code="red"}else{s.code="green"}


  rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")


 rmarkdown::render(rmd.path, output_dir=dirname(path.name), output_file=paste(basename(tools::file_path_sans_ext(path.name)), "_QAQC_", Sys.Date(), ".html", sep=''))

}



SwitchMatch=function(data){

  necessary=c("yr", "se", "mo", "strat", "da", "grp", "sppn")
  return(all(necessary %in% colnames(data)))

}


ObserverCheck=function(data){

  return(all(data$Observer==toupper(data$Observer)))

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

bad=suppressWarnings(which(is.na(as.numeric(col.data))))
#bad=which(is.na(as.numeric(col.data)))

return(list("fail"=any(bad),"bad"=bad))

}


UnitCheck=function(data){

units=c("single", "pair", "flkdrake", "open")

bad=data[which(!(data$Obs_Type %in% units)),]

return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}

SeatCheck=function(data){

  seats=c("LF", "RF", "LR", "RR")


  if("se" %in% colnames(data)){
  bad=data[which(!(data$se %in% seats)),]
  }

  if("Seat" %in% colnames(data)){
    bad=data[which(!(data$Seat %in% seats)),]
    }

  return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}


SpeciesCheck=function(data){



  if("sppn" %in% colnames(data)){
    bad=data[which(!(data$sppn %in% sppntable$QAQC) & !(data$sppn %in% sppntable$INPUT)),]

    change=data[which(data$sppn %in% sppntable$INPUT & (!(data$sppn %in% sppntable$QAQC))),]

  }

  if("Species" %in% colnames(data)){


    bad=data[which(!(data$Species %in% sppntable$QAQC) & !(data$Species %in% sppntable$INPUT)),]

    change=data[which(data$Species %in% sppntable$INPUT & (!(data$Species %in% sppntable$QAQC))),]

    }


  return(list("fail"=(length(bad[,1])!=0), "bad"=bad, "change"=change))



}


SwanCheck=function(data){

  nest=data[data$Species=="TUNE" | data$Species=="TRNE",]
  swan=data[data$Species=="TUSW" | data$Species=="TRSW",]



  bad=nest[which(nest$Obs_Type != "open"),]
  missing=nest[which(!(nest$Lat %in% swan$Lat & nest$Lon %in% swan$Lon)),]

  return(list("fail"=(length(bad[,1])!=0 | length(missing[,1])!=0), "bad"=bad, "missing"=missing))

}






ObsByYear=function(path.name){

  #created to split a master file out into observer/year specific files

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


CommonFix=function(data, fix){

  #change seats to uppercase, fix swapping (FR to RF, etc)
  if("Seat" %in% fix){

    data$Seat=toupper(data$Seat)

    data$Seat[data$Seat=="FL"]="LF"
    data$Seat[data$Seat=="FR"]="RF"
    data$Seat[data$Seat=="RL"]="LR"

  }


  if("Observer" %in% fix){

    data$Observer=toupper(data$Observer)

  }



}

