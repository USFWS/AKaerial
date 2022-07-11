




#' Create a .zip file of the associated observation files, design transects, and design strata for a project
#'
#' AerialDataZip will combine the observation files, design transects, and design strata for a range of years for a survey and
#'  combine them into a .zip file created (by default) into the current working directory.
#'
#' AerialDataZip is designed to quickly combine "official" data files into a .zip file for distribution.  It will
#' draw from the MasterFileList and output to the current working directory, unless another path is specified.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The abbreviation for the project.
#' @param drive The drive letter referring to the location of the Waterfowl directory (for example, "K:" or "Q:").
#' @param output.folder The folder for the output .zip file.
#'
#' @return None, but a .zip output file is written to the output.folder or working directory.
#'
#' @examples
#'  AerialDataZip(area="YKD", drive="T:", year=c(2000:2019))
#'
#' @export
AerialDataZip=function(area = "none", drive = "K:", output.folder = "none", year){

  MasterFileList$DRIVE = drive

  if(output.folder=="none"){output.folder=getwd()}

  entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR %in% year,]

  now.skip=0
  rep=1

  for (i in 1:length(entries[,1])){

    strata.path=paste(entries$DRIVE[i], entries$STRATA[i], sep="")

    data.path=paste(entries$DRIVE[i], entries$OBS[i], sep="")

    transect.path=paste(entries$DRIVE[i], entries$TRANS[i], sep="")

    if(!file.exists(data.path)){next}
    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}



    if(i==1){
    files.to.zip = c(strata.path, data.path, transect.path)
    }

    if(i>1){
      files.to.zip = c(files.to.zip, strata.path, data.path, transect.path)

       }



  }

  zip.name=paste(area,min(year),max(year),"Archive.zip", sep="_")

  zip(zipfile=zip.name, files=files.to.zip)

  invisible(files.to.zip)

}


