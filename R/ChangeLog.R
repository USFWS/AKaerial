



CheckData <- function (file.name, out.path=getwd()) {

  type <- file_ext(file.name)

  if(type == "txt") {loc.data <- read.table(file.name, header=TRUE)}
  if(type == "csv") {loc.data <- read.csv(file.name, header=TRUE)}

  SpatialNA(loc.data)
  ShouldBe(loc.data)
  return(loc.data)
}


ShouldBe <- function (sppn) {


for (i in 1:length(sppn)){

  if (sppn[i] %in% sppntable$should.be){

    next}
  if (sppn[i] %in% sppntable$sppn){

    should.be=sppntable$should.be[sppntable$sppn==sppn[i]]

    #print(paste("Changed species code", i, "from", sppn[i], "to", should.be))
    sppn[i] = as.character(should.be)

  }
  else {

    print(paste("Entry", i, sppn[i], "was not handled."))
  }

}

invisible(sppn)

}






SpatialNA <- function (na.data) {

  long.na <- which(scores(na.data$long, type="z", prob=0.9999))
  lat.na <- which(scores(na.data$lat, type="z", prob=0.9999))


  long.changes <- data.frame(id=long.na,
                              old.long=na.data$long[long.na],
                             old.lat=na.data$lat[long.na],
                             new.long=rep(0,length(long.na)),
                             new.lat=rep(0,length(long.na))
                             )



  lat.changes <- data.frame(id=lat.na,
                             old.long=na.data$long[lat.na],
                             old.lat=na.data$lat[lat.na],
                             new.long=rep(0,length(lat.na)),
                             new.lat=rep(0,length(lat.na))
                            )

  na.data$long[which(scores(na.data$long, type="z", prob=0.9999))] <- NA


  na.data$lat[which(scores(na.data$lat, type="z", prob=0.9999))] <- NA






  na.data$lat <- na.approx(na.data$lat)
  na.data$long <- na.approx(na.data$long)

  long.changes$new.long=na.data$long[long.changes$id]
  long.changes$new.lat=na.data$lat[long.changes$id]

  lat.changes$new.long=na.data$long[lat.changes$id]
  lat.changes$new.lat=na.data$lat[lat.changes$id]

  #print(long.changes)
  #print(lat.changes)

  write.table(long.changes, file=paste("long_changes", format(Sys.time(), "%Y_%m_%d"), ".txt", sep = ""))

  invisible(na.data)
}




