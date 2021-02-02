


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
GatherData=function(area = "none", drive = "K:", ouput.folder = "none"){

  MasterFileList$DRIVE = drive

  if(output.folder=="none"){output.folder=getwd()}

  if(area %in% c("YKD", "YKG")){
    year.panel = data.frame(
      year = c(1999:2019),
      panel = c(rep(c("A","B","C","D"),5),"A")
      )
    }

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year.panel$year,]

  now.skip=0
  rep=1

  for (i in 1:length(entries[,1])){


    if(entries$YEAR[i]==now.skip){next}

    if(entries$COMBINE[i]==1){

      if(area=="YKD" || area=="YKDV" || area=="KIG"){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKD_2001_QCObs_Pooled.csv", sep="")
        now.skip=entries$YEAR[i]
      }

      if(area=="ACP" & rep==1){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatLF.csv", sep="")

      }

      if(area=="ACP" & rep==2){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/ACP_Survey/Data/ACP_2010_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1986){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1986_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(area=="YKG" & rep==1 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatLF.csv", sep="")
      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1989){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1989_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==1997){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_1997_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="YKG" & rep==1 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatLF.csv", sep="")

      }

      if(area=="YKG" & rep==2 & entries$YEAR[i]==2005){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Data/YKG_2005_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }


      if(area=="CRD" & rep==1 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1988){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1988_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }
      if(area=="CRD" & rep==1 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatLF.csv", sep="")

      }

      if(area=="CRD" & rep==2 & entries$YEAR[i]==1998){
        data.path=paste(entries$DRIVE[i], "/Waterfowl/CRD_Survey/Data/CRD_1998_QCObs_SeatRF.csv", sep="")
        now.skip=entries$YEAR[i]
        rep=3
      }

      if(rep==1){rep=2}
      if(rep==3){rep=1}
    }

    if(entries$COMBINE[i]!=1){data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")}

    #strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")
    strata.path=paste(entries$DRIVE[i], "/Waterfowl/YKD_Coastal/Design_Files/Design_Strata/YK_DesignStrata.shp", sep="")

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    data=DataSelect(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path)

    data$design@data$UNIQUE=paste(data$design@data$OBJECTID, data$design@data$STRATNAME, data$design@data$SPLIT, sep="")

    data$obs$Panel=year.panel$panel[year.panel$year == data$obs$Year[1]]

    data$obs$Julian=as.numeric(format(as.Date(paste(data$obs$Year, data$obs$Month, data$obs$Day, sep="-")), "%j"))

    for(j in 1:length(data$transect$Year)){

      data$transect$Unique[j]=paste(data$design@data$UNIQUE[data$design@data$STRATNAME==data$transect$strata[j] &
                                                              data$design@data$SPLIT==data$transect$ctran[j]][1], year.panel$panel[year.panel$year==data$transect$Year[j], sep="")
      data$transect$Trans_Lat[j]=data$design@data$mid.Lat[data$design@data$STRATNAME==data$transect$strata[j] &
                                                            data$design@data$SPLIT==data$transect$ctran[j]][1]

      data$transect$Mean_Obs_Lat[j]=mean(data$obs$Lat[data$obs$ctran==data$transect$ctran[j]], na.rm = TRUE)

      data$transect$Mean_Julian[j]=mean(data$obs$Julian[data$obs$ctran==data$transect$ctran[j]], na.rm = TRUE)

    }


    data$transect$project=area

    if(i==1){
      write.table(data$transect, paste(output.folder, "/transect.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")
      write.table(data$obs, paste(output.folder, "/obs.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")
      write.table(data$flight, paste(output.folder, "/flight.csv", sep=""), quote=FALSE, row.names=FALSE, sep=",")

    }

    if(i>1){
    write.table(data$transect, paste(output.folder, "/transect.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    write.table(data$obs, paste(output.folder, "/obs.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    write.table(data$flight, paste(output.folder, "/flight.csv", sep=""), quote=FALSE, row.names=FALSE, append=TRUE, col.names = FALSE, sep="," )
    }


  }


  invisible(year.panel)

}
