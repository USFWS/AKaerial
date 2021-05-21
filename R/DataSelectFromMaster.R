




DataSelectFromMaster <- function(year,
                                 drive="C:",
                                 strata.overwrite="none",
                                 which.obs = 1 ,
                                 area
                                ){

  MasterFileList$DRIVE=drive

  MasterFileList = MasterFileList %>%
    dplyr::filter(YEAR == year) %>%
    dplyr::filter(AREA == area)

  strata.path=paste(MasterFileList$DRIVE[which.obs], MasterFileList$STRATA[which.obs], sep="")

  data.path=paste(MasterFileList$DRIVE[which.obs], MasterFileList$OBS[which.obs], sep="")

  transect.path=paste(MasterFileList$DRIVE[which.obs], MasterFileList$TRANS[which.obs], sep="")



  if(strata.overwrite != "none"){
    strata.path=strata.overwrite
    }


  data = DataSelect(area=area,
             data.path=data.path,
             strata.path=strata.path,
             transect.path=transect.path)

  return(data)
}











