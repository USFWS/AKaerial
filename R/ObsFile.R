#' Create or append to a master csv file of observations
#'
#' ObsFile will summarize all observation information into a csv file
#'
#' ObsFile will take a spatial layers with study area stratification polygons and
#' transect lengths and summarize the observations into a csv file.  The file can either be created entirely or appended
#' to for a single year addition.  The file is intended to be an input into the estimate-generating
#' functions in AKaerial.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param method "create" or "append"
#' @param append.to File path for appending
#'
#' @return Text file (.csv) is opened, appended to, and saved, or created and saved.
#'
#' @export
ObsFile <- function(
    method = "create",
    append.to = NULL){

  if(method=="create"){

    full_effort=c()
    full_trans=c()
    full_obs=c()

    for(j in 1:4){

      if(j==1){area="YKD"}
      if(j==2){area="YKG"}
      if(j==3){area="ACP"}
      if(j==4){area="CRD"}


      if(area == "YKD"){
        year.panel = data.frame(
          year = c(1988:2019,2021:2025),
          panel = c(1985, 1989, 1989, 1989, 1989,
                    1993, 1993, 1993, 1993, 1993,
                    1998, rep(c("D","A","B","C"),6),"D", "A")
        )
        year.panel=year.panel %>% filter(year != 2011)
      }

      if(area == "YKG"){
        year.panel = data.frame(
          year = c(1985:2019,2021:2025),
          panel = c(1985, 1985, 1985, 1985,
                    1989, 1989, 1989, 1989,
                    1993, 1993, 1993, 1993, 1993,
                    1998, rep(c("D","A","B","C"),6),"D", "A")
        )

      }

      if(area == "ACP"){
        year.panel = data.frame(
          year = c(2007:2019,2022:2025),
          panel = c(rep(c("D","A","B","C"),4),"D")
        )
      }

      if(area == "CRD"){
        year.panel = data.frame(
          year = c(1986:2012, 2014:2019, 2021:2025),
          panel = c(1986, 1987,
                    1988, 1988, 1988, 1988, 1988, 1988, 1988,
                    1995,
                    rep(1996, 17), rep("A", 11))
        )
      }


      for (t in 1:length(year.panel$year)){

        entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR == year.panel$year[t],]

        strata.path=paste(entries$DRIVE[1], entries$STRATA[1], sep="")

        strata.layer=entries$STRATA_LAYER[1]

        transect.path=paste(entries$DRIVE[1], entries$TRANS[1], sep="")

        layer.path=entries$LAYER[1]


    now.skip=0
    rep=1

    for (i in 1:length(entries[,1])){


      if(entries$YEAR[i]==now.skip){next}

      if(entries$COMBINE[i]==1){

        if(area=="YKD" || area=="YKDV" || area=="KIG"){
          data.path=paste(entries$DRIVE[i], "/final_data/YKD_2001_QCObs_Pooled.csv", sep="")
          now.skip=entries$YEAR[i]
        }

        if(area=="ACP" & rep==1){
          data.path=paste(entries$DRIVE[i], "/final_data/ACP_2010_QCObs_SeatLF.csv", sep="")

        }

        if(area=="ACP" & rep==2){
          data.path=paste(entries$DRIVE[i], "/final_data/ACP_2010_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }

        if(area=="YKG" & rep==1 & entries$YEAR[i]==1986){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1986_QCObs_SeatLF.csv", sep="")

        }

        if(area=="YKG" & rep==2 & entries$YEAR[i]==1986){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1986_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }

        if(area=="YKG" & rep==1 & entries$YEAR[i]==1989){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1989_QCObs_SeatLF.csv", sep="")
        }

        if(area=="YKG" & rep==2 & entries$YEAR[i]==1989){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1989_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }


        if(area=="YKG" & rep==1 & entries$YEAR[i]==1997){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1997_QCObs_SeatLF.csv", sep="")

        }

        if(area=="YKG" & rep==2 & entries$YEAR[i]==1997){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_1997_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }


        if(area=="YKG" & rep==1 & entries$YEAR[i]==2005){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_2005_QCObs_SeatLF.csv", sep="")

        }

        if(area=="YKG" & rep==2 & entries$YEAR[i]==2005){
          data.path=paste(entries$DRIVE[i], "/final_data/YKG_2005_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }


        if(area=="CRD" & rep==1 & entries$YEAR[i]==1988){
          data.path=paste(entries$DRIVE[i], "/final_data/CRD_1988_QCObs_SeatLF.csv", sep="")

        }

        if(area=="CRD" & rep==2 & entries$YEAR[i]==1988){
          data.path=paste(entries$DRIVE[i], "/final_data/CRD_1988_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }
        if(area=="CRD" & rep==1 & entries$YEAR[i]==1998){
          data.path=paste(entries$DRIVE[i], "/final_data/CRD_1998_QCObs_SeatLF.csv", sep="")

        }

        if(area=="CRD" & rep==2 & entries$YEAR[i]==1998){
          data.path=paste(entries$DRIVE[i], "/final_data/CRD_1998_QCObs_SeatRF.csv", sep="")
          now.skip=entries$YEAR[i]
          rep=3
        }

        if(rep==1){rep=2}
        if(rep==3){rep=1}
      }

      if(entries$COMBINE[i]!=1){data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")}


      if(!file.exists(data.path)){next}
      if(!file.exists(strata.path)){next}
      if(!file.exists(transect.path)){next}



      data=DataProcess(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path,
                       strata.id="STRATNAME", strata.layer=strata.layer, transect.layer=layer.path, retain="liberal")

      print(paste(data$obs$Year[1], area, data$obs$Observer[1], sep=" "))

      data$obs = data$obs %>% filter(!(is.na(Month)))
      if("closest" %in% colnames(data$obs)){data$obs=data$obs %>% select(-closest)}
      data$obs$Survey = area

      trans = data$transect
      trans$area = units::drop_units(trans$area)
      trans$Seat = data$obs$Seat[1]
      trans$Survey = area

      effort = st_drop_geometry(data$flight)
      effort = effort %>% select(Strata, Original, Length, SampledArea, ctran, Year, Observer, Seat)
      effort$Length = units::drop_units(effort$Length)
      effort$SampledArea = units::drop_units(effort$SampledArea)
      effort$Survey = area

      full_obs = rbind(full_obs, data$obs)
      full_effort=rbind(full_effort, effort)
      full_trans=rbind(full_trans, trans)

    } #end i (observer)

      } #end t (year)

    } #end j (survey)

    MasterObs = full_obs

    MasterSummary = full_trans

    MasterEffort = full_effort

    write.csv(MasterObs, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterObs.csv", quote = FALSE, row.names = FALSE)

    save(MasterObs, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterObs.rda")

    write.csv(MasterEffort, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterEffort.csv", quote = FALSE, row.names = FALSE)

    save(MasterEffort, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterEffort.rda")

    write.csv(MasterSummary, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterSummary.csv", quote = FALSE, row.names = FALSE)

    save(MasterSummary, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterSummary.rda")

  } #end method=create

}
