#' Create a master table of estimates for an area
#'
#' EstimatesTable will calculate index estimates over a range of years for a given area
#'
#' EstimatesTable is coded to use a single observer some years and pool more than 2 observers in other years.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  }
#' @param year The range of years to provide estimates over
#'
#' @return List object with 3 elements: \enumerate{
#' \item output.table Observer-specific estimates
#' \item expanded.table Deeper level index information by transect, strata, species, and observer
#' \item combined Combined crew index estimates by year
#' }
#'
#' @export
EstimatesTable=function(area, year, sample="full", n=0, seed=0, method="process", strata.id="STRATNAME", retain="liberal"){

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]

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

    strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    layer.path=entries$LAYER[i]

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    print(data.path)

    if(method == "process"){
      data=DataProcess(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path,
                       strata.id=strata.id, transect.layer=layer.path, retain=retain)
    }
    if(method == "select"){
      data=DataSelect(area=entries$AREA[i], data.path=data.path, transect.path=transect.path, strata.path=strata.path)
    }

    if(sample != "full"){

      data = TransectSample(data, sample=sample, n=n, seed=seed, plot="off")

    }


    if(method == "process"){est=TidyDensities(data, area=entries$AREA[i])}
    if(method == "select"){est=Densities(data, area=entries$AREA[i])}

    if(i==1){output.table=est$estimates
    expanded.table=est$counts.final}
    if(i>1){output.table=rbind(output.table, est$estimates)
    expanded.table=rbind(expanded.table, est$counts.final)}

  }

  output.table$area=area
  expanded.table$area=area


  combined=CombineEstimates(output.table)
  combined$area=area

  output.table = output.table %>% filter(!(Species %in% c("START", "END")))
  expanded.table = expanded.table %>% filter(!(Species %in% c("START", "END")))
  combined = combined %>% filter(!(Species %in% c("START", "END")))

  return(list(output.table=output.table, expanded.table=expanded.table, combined=combined))


}
