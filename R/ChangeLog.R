



CheckData <- function (file.name, out.path=getwd()) {

  type <- file_ext(file.name)

  if(type == "txt") {loc.data <- read.table(file.name, header=TRUE)}
  if(type == "csv") {loc.data <- read.csv(file.name, header=TRUE)}

  SpatialNA(loc.data)
  ShouldBe(loc.data)
  return(loc.data)
}


ShouldBe <- function (data) {


for (i in 1:length(data$sppn)){

  if (data$sppn[i] %in% sppntable$should.be){

    next}
  if (data$sppn[i] %in% sppntable$sppn){

    should.be=sppntable$should.be[sppntable$sppn==data$sppn[i]]

    #print(paste("Changed species code", i, "from", sppn[i], "to", should.be))
    data$sppn[i] = as.character(should.be)

  }
  else {

    print(paste("Entry", i, data$sppn[i], "was not handled."))
  }

}

invisible(data)

}






SpatialNA <- function (na.data, method="standard") {

  lon.na <- which(scores(na.data$Lon, type="z", prob=0.998))
  lat.na <- which(scores(na.data$Lat, type="z", prob=0.998))


  problems=data.frame(record=c(lon.na, lat.na), value=c(na.data$Lon[lon.na], na.data$Lat[lat.na]), type=c(rep("Lon", length(lon.na)), rep("Lat", length(lat.na))))

  if(length(problems[,1])==0){problems=NA}


  lon.changes <- data.frame(id=lon.na,
                              old.lon=na.data$Lon[lon.na],
                             old.lat=na.data$Lat[lon.na],
                             new.lon=rep(0,length(lon.na)),
                             new.lat=rep(0,length(lon.na))
                             )



  lat.changes <- data.frame(id=lat.na,
                             old.lon=na.data$Lon[lat.na],
                             old.lat=na.data$Lat[lat.na],
                             new.lon=rep(0,length(lat.na)),
                             new.lat=rep(0,length(lat.na))
                            )




  #na.data$lon[which(scores(na.data$Lon, type="z", prob=0.9999))] <- NA

  na.data$Lon[lon.na] <- NA



  #na.data$lat[which(scores(na.data$lat, type="z", prob=0.9999))] <- NA

  na.data$Lat[lat.na] <- NA


  na.data$Lat <- na.approx(na.data$Lat, na.rm=FALSE)
  na.data$Lon <- na.approx(na.data$Lon, na.rm=FALSE)


  lon.changes$new.lon=na.data$Lon[lon.changes$id]
  lon.changes$new.lat=na.data$Lat[lon.changes$id]

  lat.changes$new.lon=na.data$Lon[lat.changes$id]
  lat.changes$new.lat=na.data$Lat[lat.changes$id]

  #print(lon.changes)
  #print(lat.changes)

  if(method != "greenlight") {write.table(lon.changes, file=paste("lon_changes", format(Sys.time(), "%Y_%m_%d"), ".txt", sep = ""))}



  if(method == "greenlight") {invisible(problems)}

  else {invisible(na.data)}
}




