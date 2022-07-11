
#' Combine and parse raw WBPHS data files
#'
#' SplitWBPHS will combine multiple observation files and split into annual observer files
#'
#' SplitWBPHS takes a folder of raw text files (generally at the transect or stratum level) and combines them into an annual data file
#' for an observer using the naming convention: WBPHS_YYYY_RawObs_NNN.csv, where YYYY is the 4 digit year and NNN is 3 character initials.
#' Columns must be in the following order: Year, Month, Day, Seat, Observer, Stratum, Transect, Segment, Flight_Dir, A_G_Name, Wind_Dir,
#' Wind_Vel, Sky, Filename, Lat, Lon, Time, Delay, Species, Num, Obs_Type.
#' It will also add the columns Behavior, Distance, Code, and Notes that aren't used in the standard WBPHS protocol, but have become
#' the standard in Alaska Region aerial surveys.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param folder.path The directory path to the folder for input and output files.
#'
#' @return Year-Observer combinations of data in csv format to folder.path
#'
#' @export
SplitWBPHS= function(folder.path){

  setwd(folder.path)

  full.data=data.frame(
    "Year"=numeric(),
    "Month"=numeric(),
    "Day"=numeric(),
    "Seat"=character(),
    "Observer"=character(),
    "Stratum"=character(),
    "Transect"=character(),
    "Segment"=character(),
    "Flight_Dir"=numeric(),
    "A_G_Name"=character(),
    "Wind_Dir"=character(),
    "Wind_Vel"=numeric(),
    "Sky"=character(),
    "Filename"=character(),
    "Lat"=numeric(),
    "Lon"=numeric(),
    "Time"=numeric(),
    "Delay"=numeric(),
    "Species"=character(),
    "Num"=numeric(),
    "Obs_Type"=character(), stringsAsFactors = FALSE)




  file_list <- list.files(folder.path)

  for (file in file_list){


    type <- file_ext(file)

    if(type == "docx" || type=="txt" || type=="csv") {
      print(paste("Skipped", file))
      next}


    print(paste("Included file", file, "successfully."))

    temp.data <-read.csv(file, header=FALSE)
    colnames(temp.data)=colnames(full.data)
    full.data<-rbind(full.data, temp.data)


  }

  full.data$Behavior=NA
  full.data$Distance=NA
  full.data$Code=1
  full.data$Notes=NA
  full.data$Observer=toupper(full.data$Observer)

  obs.list=unique(full.data$Observer)

  for (i in 1:length(obs.list)){

    temp.data=full.data[full.data$Observer==obs.list[i],]

    yr.list=unique(temp.data$Year)

    for (k in 1:length(yr.list)){

    #  write.csv(temp.data[temp.data$Year==yr.list[k],], paste(folder.path, "/WBPHS_", yr.list[k], "_RawObs_", obs.list[i], ".csv", sep=""),
    #            row.names=FALSE, quote=FALSE)

      write.csv(temp.data[temp.data$Year==yr.list[k],], paste("K:/Waterfowl/WBPHS/Data/Raw_Survey_Data", "/WBPHS_", yr.list[k], "_RawObs_", obs.list[i], ".csv", sep=""),
                          row.names=FALSE, quote=FALSE)

    }
  }


}

