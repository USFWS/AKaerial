#' Gather all data for a project and add unique design panel information
#'
#' GatherData will compile all data for a project over its history, add transect panel information (if available), and a unique transect identifier
#'
#' GatherData is designed to compile all data over a range of years for a project with a given design transect layer and design
#' stratification.  It will add a series of columns to the resulting data that are used to derive the unique identifier for a transect
#' that may have also occurred in other years (for a panel design).
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The abbreviation for the project.
#' @param drive The drive letter referring to the location of the Waterfowl directory (for example, "K:" or "Q:").
#' @param output.folder The folder for 3 output files: flight.csv, obs.csv, and transect.csv, defaults to the working directory.
#'
#' @return None, but 3 output files are written to the output.folder or working directory.
#'
#' @examples
#'  GatherData(area="YKD", drive="T:")
#'
#' @export
GatherData=function(area = "none", drive = "K:", output.folder = "none", method="repo"){

  MasterFileList$DRIVE = drive

  if(output.folder=="none"){output.folder=getwd()}

  if(area == "YKD"){
    year.panel = data.frame(
      year = c(1988:2019,2021:2024),
      panel = c(1988:1998, rep(c("A","B","C","D"),6),"A")
      )
    }

  if(area == "YKG"){
    year.panel = data.frame(
      year = c(1985:2019,2021:2024),
      panel = c(1985:1998, rep(c("A","B","C","D"),6),"A")
    )
  }

  if(area == "ACP"){
    year.panel = data.frame(
      year = c(2007:2019,2022:2024),
      panel = c(2007:9, rep(c("A","B","C","D"),6),"A")
    )
  }

  if(area == "CRD"){
    year.panel = data.frame(
      year = c(1986:2012, 2014:2019, 2021:2024),
      panel = c("A", "B", "C","D","E","F","G","H","I",rep("J", 18), rep("K", 10))
    )
  }

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year.panel$year,]

  now.skip=0
  rep=1

  for (i in 1:length(entries[,1])){


    data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")

    # if(area %in% c("YKG", "YKD")){
    # strata.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Design_Files/Design_Strata/YK_DesignStrata.shp", sep="")
    # if(method=="repo"){strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")}
    # }
    #
    # if(area %in% c("CRD")){
    #   strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")
    # }

    strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    layer.path = entries$LAYER[i]

    data=DataProcess(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path,
                    transect.layer=layer.path)

    #data$design$UNIQUE=paste(data$design$OBJECTID, data$design$STRATNAME, data$design$SPLIT, sep="")

    data$obs = data$obs %>% filter(!(is.na(Month)))

    data$obs$Panel=year.panel$panel[year.panel$year == data$obs$Year[1]]

    #data$obs$Julian=as.numeric(format(as.Date(paste(data$obs$Year, data$obs$Month, data$obs$Day, sep="-")), "%j"))

    # for(j in 1:length(data$transect$Year)){
    #
    #   data$transect$Seat[j]=as.character(data$flight$Seat[data$flight$PartOf==data$transect$ctran[j]])
    #
    #   data$transect$Unique[j]=paste(data$design$UNIQUE[data$design$STRATNAME==data$transect$strata[j] &
    #                                                           data$design$SPLIT==data$transect$ctran[j]][1], year.panel$panel[year.panel$year==data$transect$Year[j]], sep="")
    #   data$transect$Trans_Lat[j]=data$design$mid.Lat[data$design$STRATNAME==data$transect$strata[j] &
    #                                                         data$design$SPLIT==data$transect$ctran[j]][1]
    #
    #   data$transect$Mean_Obs_Lat[j]=mean(data$obs$Lat[data$obs$ctran==data$transect$ctran[j]], na.rm = TRUE)
    #
    #   data$transect$Mean_Julian[j]=mean(data$obs$Julian[data$obs$ctran==data$transect$ctran[j]], na.rm = TRUE)
    #
    # }


    data$transect$project=area
    data$flight = data$flight %>% select(-Shape)

    if(i==1){
      write.table(data$transect, paste(output.folder, "/", area, "transect.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")
      write.table(data$obs, paste(output.folder, "/", area, "obs.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")
      write.table(data$flight, paste(output.folder, "/", area, "flight.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")

    }

    if(i>1){
    write.table(data$transect, paste(output.folder, "/", area, "transect.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    write.table(data$obs, paste(output.folder, "/", area, "obs.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    write.table(data$flight, paste(output.folder, "/", area, "flight.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    }


  }


  invisible(year.panel)

}
