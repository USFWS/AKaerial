

GreenLight=function(path.name, area){



  type <- file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE)}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "BPOP")){

    print("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, BPOP.")
    break
  }

  latlong=SpatialNA(dat1,method="greenlight")


  if(is.na(latlong)){
  n.latlong=0
  s.latlong="green"
  }
  else {
  n.latlong=length(latlong[,1])
  s.latlong="yellow"
  }

  test.colmatch=ColMatch(data)

  if(test.colmatch==TRUE){s.colmatch="green"}
  else{s.colmatch="red"}






  rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

  render(rmd.path, output_dir=getwd())

}




ColMatch=function(data){

  necessary=c("yr", "se", "obs", "strat", "tran", "lat", "long", "sppn", "grp", "unit")
  return(all(necessary %in% colnames(data)))

}


ShouldBeNumeric=function(data){




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


