

GreenLight=function(path.name, area, report=TRUE, raw2analysis=FALSE){

  can.fix=TRUE

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


  if(area=="VIS"){necessary=c("Year",
                              "Month",
                              "Day",
                              "Seat",
                              "Observer",
                              "Stratum",
                              "Transect",
                              "Segment",
                              "Trans_Num",
                              "Flight_Dir",
                              "Area",
                              "Aircraft",
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
    }

  type <- tools::file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "WBPHS", "BLSC", "VIS")){

    print("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, VIS, WBPHS.")
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
  }else{s.switchmatch="red"}



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

  test.species=SpeciesCheck(data, area=area)

  s.species="test"

  if(length(test.species$change[,1])>0){s.species="yellow"}

  if(length(test.species$bad[,1])>0){s.species="red"}

  if(test.species$fail==FALSE & s.species!="yellow"){s.species="green"}

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

 if(report == TRUE){
 rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

 rmarkdown::render(rmd.path, output_dir=dirname(path.name), output_file=paste(basename(tools::file_path_sans_ext(path.name)), "_QAQC_", Sys.Date(), ".html", sep=''))
 }

  if(any("red" %in% c(s.code, s.num, s.delay, s.time, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))){
    can.fix=FALSE
    print("can't fix")
    print(c(s.code, s.num, s.delay, s.time, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))}

  if(can.fix==TRUE & raw2analysis==TRUE){

    rmd.path=system.file("rmd/raw2analysis.Rmd", package="AKaerial")

    data.obj=Raw2Analysis(data, area=area)

    data.new=data.obj$newdata
    data.new=data.new[data.new$Species != "XXXX",]
    fix=data.obj$fix

    if(area != "VIS"){
    proj=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][1]
    yr=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][2]
    type="QCObs"
    name=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][4]
    write.path=paste(dirname(dirname(dirname(path.name))), "/", proj, "_", yr, "_QCObs_", name, ".csv", sep='')
    }

    data.new$Notes=gsub(",","", data.new$Notes)

    if(area=="VIS"){
      proj=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][1]
      yr=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][2]
      region=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][3]
      aircraft=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][4]
      name=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][6]

      type="QCObs"
    write.path=paste(dirname(dirname(dirname(path.name))), "/", proj, "_", yr, "_", region, "_", aircraft, "_QCObs_", name, ".csv", sep='')
    }

    print(write.path)
    write.csv(data.new[,1:25], write.path, quote=FALSE, row.names=FALSE )

    if(area!="VIS"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj,"_", yr, "_QCLog_",name, ".html", sep=''))}
    if(area=="VIS"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj, "_", yr, "_", region, "_", aircraft, "_QCLog_", name, ".html", sep=''))}
  }


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

bad.na=suppressWarnings(which(is.na(col.data)))
bad=suppressWarnings(which(is.na(as.numeric(col.data))))
#bad=which(is.na(as.numeric(col.data)))

bad=bad[!(bad %in% bad.na)]

return(list("fail"=any(bad),"bad"=bad))

}


UnitCheck=function(data){

units=c("single", "pair", "flkdrake", "open", NA)

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


SpeciesCheck=function(data, area){


if(area!="WBPHS"){

  if("sppn" %in% colnames(data)){
    bad=data[which(!(data$sppn %in% sppntable$QAQC) & !(data$sppn %in% sppntable$INPUT)),]

    change=data[which(data$sppn %in% sppntable$INPUT & (!(data$sppn %in% sppntable$QAQC))),]

  }

  if("Species" %in% colnames(data)){


    bad=data[which(!(data$Species %in% sppntable$QAQC) & !(data$Species %in% sppntable$INPUT)),]

    change=data[which(data$Species %in% sppntable$INPUT & (!(data$Species %in% sppntable$QAQC))),]

  }

}

  if(area=="WBPHS"){


    if("sppn" %in% colnames(data)){
      bad=data[which(!(data$sppn %in% WBPHSsppntable$QAQC) & !(data$sppn %in% WBPHSsppntable$INPUT)),]

      change=data[which(data$sppn %in% WBPHSsppntable$INPUT & (!(data$sppn %in% WBPHSsppntable$QAQC))),]

    }

    if("Species" %in% colnames(data)){


      bad=data[which(!(data$Species %in% WBPHSsppntable$QAQC) & !(data$Species %in% WBPHSsppntable$INPUT)),]

      change=data[which(data$Species %in% WBPHSsppntable$INPUT & (!(data$Species %in% WBPHSsppntable$QAQC))),]

    }




  }


  return(list("fail"=(length(bad[,1])!=0), "bad"=bad, "change"=change))



}


SwanCheck=function(data){

  nest=data[data$Species=="TUNE" |
              data$Species=="TRNE" |
              data$Species=="SWANN" |
              data$Species=="SWNE" |
              data$Species=="SW.N" |
              data$Species=="TS.N" |
              data$Species == "TSNE" |
              data$Species == "tune" |
              data$Species == "TUSWN" |
              data$Species == "SWANNEST",]

  swan=data[data$Species=="TUSW" |
              data$Species=="TRSW" |
              data$Species=="SWAN" |
              data$Species=="TRUS" |
              data$Species=="Tusw" |
              data$Species=="TUsw" |
              data$Species=="tusw",]

  bad=nest[which(nest$Obs_Type != "open" | nest$Num != 1),]
  bad.index=as.numeric(rownames(bad))

  missing=nest[which(!(nest$Lat %in% swan$Lat & nest$Lon %in% swan$Lon)),]
  missing.index=as.numeric(rownames(missing))


  return(list("fail"=(length(bad[,1])!=0 | length(missing[,1])!=0), "bad"=bad, "missing"=missing, "bad.index"=bad.index, "missing.index"=missing.index))

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


CommonFix=function(data, fix, area){

  #change seats to uppercase, fix swapping (FR to RF, etc)
  if("Seat" %in% fix){

    data$Seat=toupper(data$Seat)

    data$Seat[data$Seat=="FL"]="LF"
    data$Seat[data$Seat=="FR"]="RF"
    data$Seat[data$Seat=="RL"]="LR"

  }

  #change Observer to uppercase
  if("Observer" %in% fix){

    data$Observer=toupper(data$Observer)

  }


  if("Swan" %in% fix){

    swan.probs=SwanCheck(data)

    for(i in swan.probs$missing.index){

      new.row=data[i,]

      if(new.row$Num==1){new.row$Obs_Type="single"}
      if(new.row$Num==2){new.row$Obs_Type="pair"}
      new.row$Species="SWAN"
      new.row$Num=1

      data=rbind(data, new.row)

    }

    for(j in swan.probs$bad.index){
      data$Obs_Type[j]="open"
      data$Num[j]=1

    }

  }

  if("Obs_Type" %in% fix){


    for(k in 1:length(data$Obs_Type)){

      if(data$Species[k] %in% c("TUNE", "TRNE", "SWANN", "SWNE", "SW.N", "TS.N", "TSNE", "tune", "TUSWN"))
      {
        data$Num[k]=1
        data$Obs_Type[k]="open"
        next
        }

      if(data$Num[k]==1){

        data$Obs_Type[k]="single"

      }


      if(data$Num[k]==2){

        data$Obs_Type[k]="pair"
        data$Num[k]=1

      }

      if(data$Num[k] > 2){

        data$Obs_Type[k]="open"

      }

    }


  }

  if("Species" %in% fix){

  #data$OldSpecies=data$Species

    if(area!="WBPHS"){

  for(n in 1:length(data$Species)){

    data$Species[n]=sppntable$QAQC[sppntable$INPUT==data$Species[n]]

  }
    }


    if(area == "WBPHS"){

      for(n in 1:length(data$Species)){

        data$Species[n]=WBPHSsppntable$QAQC[WBPHSsppntable$INPUT==data$Species[n]]

      }
    }


    }


  return(data)

}


Raw2Analysis=function(data, area){
  fix=c("Species")

  if(length(unique(data$Obs_Type[!is.na(data$Obs_Type)])) <= 1){fix=c(fix, "Obs_Type")}

  seat.test=SeatCheck(data)
  if(seat.test$fail==TRUE){fix=c(fix, "Seat")}

  obs.test=ObserverCheck(data)
  if(obs.test==FALSE){fix=c(fix, "Observer")}

  swan.test=SwanCheck(data)
  if(swan.test$fail==TRUE){fix=c(fix, "Swan")}

  data=CommonFix(data=data, fix=fix, area=area)

  return(list("newdata"=data, "fix"=fix))

}


