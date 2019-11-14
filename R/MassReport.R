

#' Apply \code{\link{Greenlight}} function to an entire folder
#'
#' MassReport will scan a folder for .csv data files and apply the \code{\link{Greenlight}}
#' function to each.
#'
#' MassReport is designed to produce QAQC reports for a large number of files based on a set of established criteria for data integrity.
#' It can also create the files used for analysis (those with extraneous species codes removed) based on the method chosen.
#' It was created to avoid single calls to Greenlight when changes are implemented in the QAQC process that need to be applied to all data files at once, such as
#' including or excluding a species in the generated sharable analysis files.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param folder.path The path name to the folder containing the raw .csv files.
#'  Either folder.path or area must be specified.
#' @param area The area code for dedicated MBM Alaska region surveys.  Either folder.path or area must be specified.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, for either geese or ducks
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  }
#' @param method Specify \code{greenlight}, \code{data}, or \code{both}.
#'  \itemize{
#'  \item \code{method=greenlight} will run the standard \code{\link{Greenlight}} report for each data file
#'  \item \code{method=data} will generate the QAQC data file (if possible) for each data file
#'  \item \code{method=both} will run the standard \code{\link{Greenlight}} report for each data file and produce each associated QAQC data file
#'  }
#'
#' @return None
#'
#' @examples
#'  MassReport(area="ACP", method="both")
#'
#' @export
MassReport=function(folder.path=NA, area=NA, method="both"){

#specify folder if none specified

if(is.na(folder.path)){
if(area=="YKD"){folder.path="Q:/Waterfowl/YKD_Coastal/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="ACP"){folder.path="Q:/Waterfowl/ACP_Survey/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="CRD"){folder.path="Q:/Waterfowl/CRD_Survey/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="WBPHS"){folder.path="Q:/Waterfowl/WBPHS/Data/Observer Transcribed Data/Processed/"}
}

#get all files

file.set=list.files(folder.path)

#remove anything that isn't csv

file.set=file.set[tools::file_ext(file.set)=="csv"]

#file.set=file.set[substr(file.set,1,3)=="YKG"]

write.table(file.set, paste(folder.path, "filelist.csv", sep=""), quote=F, row.names = F, col.names = F, sep="," )

for (i in 1:length(file.set)){

  path.name=paste(folder.path, file.set[i], sep='')

  if(method=="both" || method=="greenlight"){GreenLight(path.name, area=area, raw2analysis = FALSE, report=TRUE)}
  if(method=="both" || method=="data"){GreenLight(path.name, area=area, raw2analysis = TRUE, report=FALSE)}
}


}



#' Pool data from multiple observers across all MBM surveys where appropriate
#'
#' MassPool will selectively apply the \code{\link{PoolData}} function to all known survey, year, and observer
#' combinations that need pooling.
#'
#' Pooling data files becomes necessary when observers and pilots switch seats or
#' are replaced during a survey.  The resulting files are generally pooled by seat side (left or right).
#' MassPool currently applies the \code{\link{PoolData}} function to the following data files:
#' \itemize{
#' \item 1986 YKG files from observers C. Lensink and K. Bollinger are pooled into a right front seat file.
#' Lensink observed from 6-2-1986 through 6-22-1986 with the exception of 6-13-1986.  Bollinger observed on 7 design transects on 6-13-1986.
#' W. Butler was the only pilot for the 1986 YKG survey.
#' \item 1988 CRD files
#' }
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#'



MassPool=function(){

  PoolData(year=2001, area="YKD")
  PoolData(year=1986, area="YKG")
  PoolData(year=1989, area="YKG")
  PoolData(year=1997, area="YKG")
  PoolData(year=2005, area="YKG")
  PoolData(year=1988, area="CRD")
  PoolData(year=1998, area="CRD")
  PoolData(year=2010, area="ACP")


}
