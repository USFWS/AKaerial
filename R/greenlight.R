

#' QA/QC checks for standardized aerial survey data
#'
#' GreenLight will take input data, check for errors, create a summary report, and optionally output a new data file.
#'
#' GreenLight is designed to automate common data QA/QC functions that are normally done live by an observer.
#' In the future, these checks will use a specific set of fields derived from a generic aerial survey protocol, but
#' this protocol does not currently exist, so instead the checks provided here are simply to streamline the
#' creation of population estimates using this R package and provide some consistency in the data fields before
#' a data set is shared with others.  The specific tests performed are:
#' \enumerate{
#' \item Column Names - Are all required columns present and named correctly?  Necessary columns are:
#'   \describe{
#'   \item{Year}{4 digit integer representing the year of the observation}
#'   \item{Month}{2 digit integer representing the month of the observation}
#'   \item{Day}{1 or 2 digit integer representing the day of the observation}
#'   \item{Seat}{2 character representation of seat assignment; RF (right front), LF (left front), RR (right rear), or LR (left rear)}
#'   \item{Observer}{3 character initials of the observer (such as CJF, all capitalized, or C_F)}
#'   \item{Stratum}{character string (or numeric, treated as string) representing the stratum the observation was in (if known by the observer)}
#'   \item{Transect}{character string (or numeric, treated as string) of the DESIGN FILE transect number}
#'   \item{Segment}{character string (or numeric, treated as string) of the transect segment (if known)}
#'   \item{Flight_Dir}{character string of flight direction (numeric as degrees, character cardinal or intercardinal directions)}
#'   \item{A_G_Name}{character string of air to ground segment ID}
#'   \item{Wind_Dir}{character string of wind direction based on 8 point cardinal/intercardinal directions}
#'   \item{Wind_Vel}{integer representing wind speed in knots}
#'   \item{Sky}{character string representing sky condition (clear, scattered, etc.)}
#'   \item{Filename}{character string representing .wav file recording for the associated observation}
#'   \item{Lat}{floating decimal representing decimal degrees of latitude in WGS84 datum}
#'   \item{Lon}{floating decimal representing decimal degrees of longitude in WGS84 datum}
#'   \item{Time}{floating decimal representing computer clock seconds past midnight}
#'   \item{Delay}{floating decimal representing the delay in clock time and GPS system time in seconds}
#'   \item{Species}{4 character string representing the acceptable species code for an observation.  See \code{\link{sppntable}}}
#'   \item{Num}{up to 5 digit integer representing the number seen}
#'   \item{Obs_Type}{character string representing observation type: \enumerate{
#'   \item single - one lone drake (dimorphic species) or lone bird (monomorphic species)
#'   \item pair - hen and drake in close association (dimorphic species) or 2 birds in close association (monomorphic species)
#'   \item open - a mixed sex flock that can't be classified as single, pair, or flkdrake
#'   \item flkdrake - 2 or more drakes in close association}}
#'   \item{Behavior}{character string representing observed behavior: diving, flying, swimming, or NA}
#'   \item{Distance}{character string representing distance from the observer: near, far, NA}
#'   \item{Code}{integer representing the use of the data in analysis: \enumerate{
#'   \item use in standard index estimate
#'   \item use as double observer only
#'   \item additional data collected but not used in analysis}}
#'   \item{Notes}{character string reserved for additional comments}
#'   }
#' \item Area - Does the area specified have a defined QAQC process? Acceptable values include: \itemize{
#'   \item YKD - Yukon Kuskokwim Delta MBM duck stratification
#'   \item WBPHS - Waterfowl Breeding Population Habitat Survey ("North American")
#'   \item YKG - Yukon Kuskokwim Delta MBM goose stratification
#'   \item BLSC - Black Scoter
#'   \item ACP - Arctic Coastal Plain
#'   \item CRD - Copper River Delta
#'   \item VIS - Aircraft Visibility}
#' \item Swans - Are swans and swan nests appropriately recorded?  Appropriate treatment is recording any observed swans separately from their nests, and nests as open 1.
#'  This applies to tundra swans (TUSW) and trumpeter swans (TRSW), and their associated nests.
#' \item Obs_Type - Are all Obs_Type recorded as one of the accepted 4 codes (single, pair, open, flkdrake)?
#' \item Seat - Are the observer seat codes recorded as one of the 4 acceptable upper case codes (RF, LF, RR, LR)?
#' \item Species - Are the species codes correct?  See \code{\link{sppntable}}.
#' \item Observer - Are all observer initials the same (1 observer per data file) and upper case?
#' \item Numeric columns - The columns Year, Month, Day, Wind_Vel, Lat, Lon, Time, Delay, Num, and Code must contain only numeric values.
#' }
#'
#' A file is given a "red light" and deemed inappropriate for analysis if it fails checks on required columns, is in an undefined area,
#' incorrect Obs_Types detected, unknown seat code, unrecognized species codes, multiple observers per file, or any of the numeric columns contain non-numerics.
#'
#' A file is given a "yellow light" that indicates inconsistencies in the file, but with known treatments by the function, if any of several common mistakes occur.
#' These include incorrect swan transcription, reversed seat codes (FR for front right instead of RF), use of older species codes, or lower case observer initials.
#'
#' If a file sufficiently passes quality checks (receives a green light) and raw2analysis = TRUE, a QCobs (archive quality) .csv file and associated report is produced 2 directories above the path.name specified.
#' This is done according to the file structure proposed in current data management plans.  If a file receives a yellow light and raw2analysis = TRUE, the associated report will detail the
#' quality checks that were failed and the specific treatment by the function of the offending data fields before creating the QCobs file.  If a preliminary report has already been produced, setting
#' report = FALSE before generating a QCobs file will stop another preliminary report from being generated.
#'
#'
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param path.name The path to the raw data file to be checked.
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta MBM duck stratification
#'  \item WBPHS - Waterfowl Breeding Population Habitat Survey ("North American")
#'  \item YKG - Yukon Kuskokwim Delta MBM goose stratification
#'  \item BLSC - Black Scoter
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item VIS - Aircraft Visibility
#'  }
#' @param report TRUE or FALSE, should a preliminary report be generated?
#' @param raw2analysis TRUE or FALSE, should the archive version of the raw data file be generated (if possible)?
#' @param archive.dir Path to the desired archive directory for the new data file.  Defaults to 3 directory levels above the input file to follow current MBM practices.
#'
#' @return None
#'
#' @examples
#'  Greenlight(path.name = "C:/DATA/MyData.csv", area = "CRD", report = TRUE, raw2analysis = FALSE)
#'
#' @export
GreenLight=function(path.name, area, report=TRUE, raw2analysis=FALSE, archive.dir = "default"){

  can.fix=TRUE

  necessary=c("Year",
              "Month",
              "Day",
              "Seat",
              "Observer",
              "Stratum",
              "Transect",
              "Segment",
              "Flight_Dir",
              "A_G_Name",
              "Wind_Dir",
              "Wind_Vel",
              "Sky",
              "Filename",
              "Lat",
              "Lon",
              "Time",
              "Delay",
              "Species",
              "Num",
              "Obs_Type",
              "Behavior",
              "Distance",
              "Code",
              "Notes")


  if(area=="VIS"){necessary=c("Year",
                              "Month",
                              "Day",
                              "Seat",
                              "Observer",
                              "Stratum",
                              "Transect",
                              "Segment",
                              "Trans_Num",
                              "Flight_Dir",
                              "Area",
                              "Aircraft",
                              "Wind_Vel",
                              "Sky",
                              "Filename",
                              "Lat",
                              "Lon",
                              "Time",
                              "Delay",
                              "Species",
                              "Num",
                              "Obs_Type",
                              "Behavior",
                              "Distance",
                              "Code",
                              "Notes")
    }

  type <- tools::file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "WBPHS", "BLSC", "VIS")){

    stop("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, VIS, WBPHS.")

  }


  #are the columns there that need to be renamed?
  test.switchmatch=SwitchMatch(data)

  if(test.switchmatch==TRUE){
    s.switchmatch="green"
  }else{s.switchmatch="red"}



  #are all of the required columns there (and named correctly)?
  test.colmatch=ColMatch(data, necessary=necessary)

  if(test.colmatch==TRUE){s.colmatch="green"}else{s.colmatch="red"}


  #are there any swan nests without associated records (missing) or swan nests recorded as
  #anything but open?

  test.swan=SwanCheck(data)

  if(test.swan$fail==TRUE){s.swan="red"}else{s.swan="green"}


  #are there only 4 possible unit types? (Obs_type = single, pair, open, flkdrake)

  test.unit=UnitCheck(data)

  if(test.unit$fail==TRUE){s.unit="red"}else{s.unit="green"}


  #are there only 4 possible seats recorded? (Seat = RF, RR, LF, LR)

  test.seat=SeatCheck(data)

  if(test.seat$fail==TRUE){s.seat="red"}else{s.seat="green"}

  #are the species codes correct?

  test.species=SpeciesCheck(data, area=area)

  s.species="test"

  if(length(test.species$change[,1])>0){s.species="yellow"}

  if(length(test.species$bad[,1])>0){s.species="red"}

  if(test.species$fail==FALSE & s.species!="yellow"){s.species="green"}

  #are the observer initials all uppercase?

  test.observer=ObserverCheck(data)

  if(test.observer==TRUE){s.observer="green"}else{s.observer="red"}


  #one observer per file?

  test.oneobserver=(length(unique(data$Observer))==1)

  if(test.oneobserver==TRUE){s.oneobserver="green"}else{s.oneobserver="red"}


  #Numeric tests- Year, Month, Day, Wind_Vel, Lat, Lon, Time, Delay, Num

  test.year=ShouldBeNumeric(data$Year)
  if(test.year$fail==TRUE){s.year="red"}else{s.year="green"}

  test.month=ShouldBeNumeric(data$Month)
  if(test.month$fail==TRUE){s.month="red"}else{s.month="green"}

  test.day=ShouldBeNumeric(data$Day)
  if(test.day$fail==TRUE){s.day="red"}else{s.day="green"}

  test.wind=ShouldBeNumeric(data$Wind_Vel)
  if(test.wind$fail==TRUE){s.wind="red"}else{s.wind="green"}

  test.lat=ShouldBeNumeric(data$Lat)
  if(test.lat$fail==TRUE){s.lat="red"}else{s.lat="green"}

  test.lon=ShouldBeNumeric(data$Lon)
  if(test.lon$fail==TRUE){s.lon="red"}else{s.lon="green"}

  test.time=ShouldBeNumeric(data$Time)
  if(test.time$fail==TRUE){s.time="red"}else{s.time="green"}

  test.delay=ShouldBeNumeric(data$Delay)
  if(test.delay$fail==TRUE){s.delay="red"}else{s.delay="green"}

  test.num=ShouldBeNumeric(data$Num)
  if(test.num$fail==TRUE){s.num="red"}else{s.num="green"}

  # test.code=ShouldBeNumeric(data$Code)
  # if(test.code$fail==TRUE){s.code="red"}else{s.code="green"}



  ## E Osnas mapping function

  if(area != "WBPHS"){

  sf.obs <- data %>% sf::st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
    sf::st_transform(crs=3338) %>%
    filter(Code == 1) #remove any non-survey or special observations

  sf.lines <- sf.obs %>%
    sf::st_transform(crs=4326) %>%
    group_split(Transect, Day) %>%
    purrr::map(points2line) %>%
    purrr::map_dfr(rbind)

  if(area=="ACP"){basemap="K:/Waterfowl/ACP_Survey/Design_Files/Design_Strata/ACP_2007to2018_DesignStrata.shp"}
  if(area %in% c("YKD", "YKG", "YKDV")){basemap="K:/Waterfowl/YKD_Coastal/Design_Files/Design_Strata/YK_DesignStrata.shp"}
  if(area=="CRD"){basemap="K:/Waterfowl/CRD_Survey/Design_Files/Design_Strata/CRD_2018_DesignStrata.shp"}

  basemap <- sf::st_read(dsn=basemap) %>%
    sf::st_transform(crs=3338)

  sf.obs <- sf.obs %>%  mutate(Day=as.character(Day))

}

  ## End mapping, plot the object in markdown

  rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

 if(report == TRUE){
 rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

 rmarkdown::render(rmd.path, output_dir=dirname(path.name), output_file=paste(basename(tools::file_path_sans_ext(path.name)), "_QAQC_", Sys.Date(), ".html", sep=''))
 }

  if(any("red" %in% c(s.num, s.delay, s.time, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))){
    can.fix=FALSE
    print("can't fix")
    print(c(s.num, s.delay, s.time, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))}

  if(can.fix==TRUE & raw2analysis==TRUE){

    rmd.path=system.file("rmd/raw2analysis.Rmd", package="AKaerial")

    data.obj=Raw2Analysis(data, area=area)

    data.new=data.obj$newdata
    data.new=data.new[data.new$Species != "XXXX",]
    fix=data.obj$fix

    if(area != "VIS"){
    proj=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][1]
    yr=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][2]
    type="QCObs"
    name=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][4]
    #fix initials cut off on initials containing "_"
    #name=substr(basename(tools::file_path_sans_ext(path.name)), nchar(basename(tools::file_path_sans_ext(path.name)))-2, nchar(basename(tools::file_path_sans_ext(path.name))))

    if(archive.dir != "default"){
    write.path=paste(archive.dir, "/", proj, "_", yr, "_QCObs_", name, ".csv", sep='')
    }else{write.path=paste(dirname(dirname(dirname(path.name))), "/", proj, "_", yr, "_QCObs_", name, ".csv", sep='')}

    }

    data.new$Notes=gsub(",","", data.new$Notes)

    if(area=="VIS"){
      proj=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][1]
      yr=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][2]
      region=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][3]
      aircraft=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][4]
      name=strsplit(basename(tools::file_path_sans_ext(path.name)),"_")[[1]][6]

      type="QCObs"

    if(archive.dir != "default"){
    write.path=paste(archive.dir, "/", proj, "_", yr, "_", region, "_", aircraft, "_QCObs_", name, ".csv", sep='')
    }else{write.path=paste(dirname(dirname(dirname(path.name))), "/", proj, "_", yr, "_", region, "_", aircraft, "_QCObs_", name, ".csv", sep='')}

    }


    print(write.path)

    if(area=="WBPHS"){write.csv(data.new[,1:25], write.path, quote=FALSE, row.names=FALSE, na="")
    }else{write.csv(data.new[,1:25], write.path, quote=FALSE, row.names=FALSE) }

    if(area!="VIS" & archive.dir != "default"){rmarkdown::render(rmd.path, output_dir=archive.dir, output_file=paste(proj,"_", yr, "_QCLog_",name, ".html", sep=''))}
    if(area=="VIS" & archive.dir != "default"){rmarkdown::render(rmd.path, output_dir=archive.dir, output_file=paste(proj, "_", yr, "_", region, "_", aircraft, "_QCLog_", name, ".html", sep=''))}
    if(area!="VIS" & archive.dir == "default"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj,"_", yr, "_QCLog_",name, ".html", sep=''))}
    if(area=="VIS" & archive.dir == "default"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj, "_", yr, "_", region, "_", aircraft, "_QCLog_", name, ".html", sep=''))}

    }


}



#' Check for the old style of transcribed data.
#'
#' SwitchMatch will test if the original style of transcribed column headings is present
#'
#' SwitchMatch is designed to check if the old style of column headings is present and the minimum set of
#' yr, se, mo, strat, da, grp, and sppn exists.  If TRUE, then ColSwitch can be used to overwrite to the current headings.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#'
#' @return TRUE/FALSE
#'
#' @export
SwitchMatch=function(data){

  necessary=c("yr", "se", "mo", "strat", "da", "grp", "sppn")
  return(all(necessary %in% colnames(data)))

}


#' Check if observer initials are uppercase.
#'
#' ObserverCheck checks to see if the observer initials were entered consistently in uppercase
#'
#' ObserverCheck checks to see if the observer initials were entered consistently in uppercase.  This is a basic QA/QC check
#' so the analysis is not split among upper and lowercase versions of the same observer.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#'
#' @return TRUE/FALSE
#'
#' @export
ObserverCheck=function(data){

  return(all(data$Observer==toupper(data$Observer)))

}

#' Switch necessary column names
#'
#' ColSwitch takes old column names and changes them to current styling
#'
#' ColSwitch will change yr, mo, da, se, strata, grp, and sppn to Year, Month, Day, Seat, Strata, Obs_Type, and Species, respectively.
#' This function was written to quickly allow an old data file to be formatted for analysis, but probably won't be used in the future.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#'
#' @return Data frame with new column names.
#'
#' @export
ColSwitch=function(data){

  colnames(data)[which(colnames(data)=="yr")]="Year"
  colnames(data)[which(colnames(data)=="mo")]="Month"
  colnames(data)[which(colnames(data)=="da")]="Day"
  colnames(data)[which(colnames(data)=="se")]="Seat"
  colnames(data)[which(colnames(data)=="strat")]="Strata"
  colnames(data)[which(colnames(data)=="grp")]="Obs_Type"
  colnames(data)[which(colnames(data)=="sppn")]="Species"

  return(data)
}

#' Check if column names are appropriate for analysis.
#'
#' ColMatch checks to see if the column headings match those needed by AKAerial for analysis.
#'
#' ColMatch is the first GreenLight QA/QC check performed on a data set.  If the column names do not match the necessary set of names in GreenLight,
#' the function will throw an error and end abruptly.  For the current list of acceptable names, see \code{\link{GreenLight}}
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#'
#' @return TRUE/FALSE
#'
#' @export
ColMatch=function(data, necessary){

  return(all(necessary %in% colnames(data)))

}

#' Check if entries in a column are all numeric
#'
#' ShouldBeNumeric checks to see if column entries are numeric values needed for analysis.
#'
#' ShouldBeNumeric is a GreenLight QA/QC check performed on a data set.  If the column entries are not numeric values it will return a "red" status.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param col.data The column to be checked.
#'
#' @return data frame of TRUE/FALSE and offending entries
#'
#' @export
ShouldBeNumeric=function(col.data){

bad.na=suppressWarnings(which(is.na(col.data)))
bad=suppressWarnings(which(is.na(as.numeric(col.data))))
#bad=which(is.na(as.numeric(col.data)))

bad=bad[!(bad %in% bad.na)]

return(list("fail"=any(bad),"bad"=bad))

}

#' Check if Obs_Type entries are all correct
#'
#' UnitCheck checks to see if Obs_Type entries are correct values needed for analysis.
#'
#' UnitCheck is a GreenLight QA/QC check performed on a data set.  If the Obs_Type entries are not single, pair, flkdrake, or open, it will return a "red" status.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param col.data The column to be checked.
#'
#' @return data frame of TRUE/FALSE and offending entries
#'
#' @export
UnitCheck=function(data){

units=c("single", "pair", "flkdrake", "open", NA)

bad=data[which(!(data$Obs_Type %in% units)),]

return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}

#' Check if Seat entries are all correct
#'
#' SeatCheck checks to see if Seat entries are correct values needed for analysis.
#'
#' SeatCheck is a GreenLight QA/QC check performed on a data set.  If the Seat entries are not LF, RF, LR, or RR, it will return a "red" status.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param col.data The column to be checked.
#'
#' @return data frame of TRUE/FALSE and offending entries
#'
#' @export
SeatCheck=function(data){

  seats=c("LF", "RF", "LR", "RR")


  if("se" %in% colnames(data)){
  bad=data[which(!(data$se %in% seats)),]
  }

  if("Seat" %in% colnames(data)){
    bad=data[which(!(data$Seat %in% seats)),]
    }

  return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}


#' Check if Species column inputs are all correct
#'
#' SpeciesCheck checks to see if Species entries are correct values needed for analysis.
#'
#' SpeciesCheck is a GreenLight QA/QC check performed on a data set.  Species entries are cross-referenced with approved entries by project in
#' \code{\link{sppntable}} and values with known treatments are marked as "yellow light" issues that will be updated.  Completely unknown species codes
#' will result in a "red" status.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param col.data The column to be checked.
#'
#' @return data frame of TRUE/FALSE, incorrect unfixable entries, and incorrect but fixable entries
#'
#' @export
SpeciesCheck=function(data, area){


if(area!="WBPHS"){

  if("sppn" %in% colnames(data)){
    bad=data[which(!(data$sppn %in% sppntable$QAQC) & !(data$sppn %in% sppntable$INPUT)),]

    change=data[which(data$sppn %in% sppntable$INPUT & (!(data$sppn %in% sppntable$QAQC))),]

  }

  if("Species" %in% colnames(data)){


    bad=data[which(!(data$Species %in% sppntable$QAQC) & !(data$Species %in% sppntable$INPUT)),]

    change=data[which(data$Species %in% sppntable$INPUT & (!(data$Species %in% sppntable$QAQC))),]

  }

}

  if(area=="WBPHS"){


    if("sppn" %in% colnames(data)){
      bad=data[which(!(data$sppn %in% WBPHSsppntable$QAQC) & !(data$sppn %in% WBPHSsppntable$INPUT)),]

      change=data[which(data$sppn %in% WBPHSsppntable$INPUT & (!(data$sppn %in% WBPHSsppntable$QAQC))),]

    }

    if("Species" %in% colnames(data)){


      bad=data[which(!(data$Species %in% WBPHSsppntable$QAQC) & !(data$Species %in% WBPHSsppntable$INPUT)),]

      change=data[which(data$Species %in% WBPHSsppntable$INPUT & (!(data$Species %in% WBPHSsppntable$QAQC))),]

    }




  }

  return(list("fail"=(length(bad[,1])!=0), "bad"=bad, "change"=change))



}


#' Check if swan and swan nest entries are all correct
#'
#' SwanCheck checks to see if swan and swan nest entries were transcribed correctly for analysis.
#'
#' SwanCheck is a GreenLight QA/QC check performed on a data set.  The correct transcription of a swan and associated nest is to
#' record the swan Obs_Type (single or pair) and then the associated nest as a separate row with Obs_Type open and Num 1.  Anything
#' else will result in "yellow" status if the treatment of the data type is known, and "red" if unknown.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#'
#' @return data frame of TRUE/FALSE and offending entries
#'
#' @export
SwanCheck=function(data){

  nest=data[data$Species=="TUNE" |
              data$Species=="TRNE" |
              data$Species=="SWANN" |
              data$Species=="SWNE" |
              data$Species=="SW.N" |
              data$Species=="TS.N" |
              data$Species == "TSNE" |
              data$Species == "tune" |
              data$Species == "TUSWN" |
              data$Species == "SWANNEST",]

  swan=data[data$Species=="TUSW" |
              data$Species=="TRSW" |
              data$Species=="SWAN" |
              data$Species=="TRUS" |
              data$Species=="Tusw" |
              data$Species=="TUsw" |
              data$Species=="tusw",]

  bad=nest[which(nest$Obs_Type != "open" | nest$Num != 1),]
  bad.index=as.numeric(rownames(bad))

  missing=nest[which(!(nest$Lat %in% swan$Lat & nest$Lon %in% swan$Lon)),]
  missing.index=as.numeric(rownames(missing))


  return(list("fail"=(length(bad[,1])!=0 | length(missing[,1])!=0), "bad"=bad, "missing"=missing, "bad.index"=bad.index, "missing.index"=missing.index))

}





#' Split an old combined data file by observer and year
#'
#' ObsByYear will take a .csv or .txt file with 2 or more observers and parse it into observer- and year-specific files
#'
#' ObsByYear will take a .csv or .txt file with 2 or more observers and parse it into observer- and year-specific files. This is designed to fix
#' older files that may have been concatenated into a master file, splitting them into separate pieces.  Since this was an ACP problem,
#' the files default to the directory "Q:/Waterfowl/Parsed/ACP_" and would need recoding for any other survey.  Individual .csv files are written to the directory.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param path.name The path to the combined file.
#'
#' @return None
#'
#' @export
ObsByYear=function(path.name){

  #created to split a master file out into observer/year specific files

  type <- file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE)}

  yr.list=unique(data$yr)

  data$obs=toupper(data$obs)

  for(year in seq_along(yr.list)){

    temp.data=data[data$yr==yr.list[year],]
    obs.list=unique(temp.data$obs)

    print(obs.list)
    print(length(obs.list))

    for (i in 1:length(obs.list)){
    write.csv(temp.data[temp.data$obs==obs.list[i],],
              file=paste("Q:/Waterfowl/Parsed/ACP_", yr.list[year], "_RawObs_", obs.list[i], ".csv", sep=""),
              row.names=FALSE,
              col.names=names(temp.data),
              quote=FALSE
              )
    }

  }


}



#' Apply "yellow light" changes to a data set
#'
#' CommonFix is used in conjunction with GreenLight to apply "no-loss" changes to a data set
#'
#' Commonfix will take "yellow light" issues and apply a known treatment to fix offending issues. The list of fixes includes \itemize{
#'   \item Seat - changes lower to uppercase, flips the characters (FR, FL, RL to RF, LF, LR)
#'   \item Observer - changes lower to uppercase
#'   \item Swan - breaks up a nest-only observation into 2 observations
#'   \item Obs_Type - changes open 2 or open 1 SWAN to pair or single, changes SWANN to open 1
#'   \item Species - changes incorrect or outdated species codes to current ones}
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be fixed.
#' @param fix The character string vector of the names of the fixes to be applied.
#' @param area The project area designation.
#'
#' @return data frame of fixed columns
#'
#' @export
CommonFix=function(data, fix, area){

  #change seats to uppercase, fix swapping (FR to RF, etc)
  if("Seat" %in% fix){

    data$Seat=toupper(data$Seat)

    data$Seat[data$Seat=="FL"]="LF"
    data$Seat[data$Seat=="FR"]="RF"
    data$Seat[data$Seat=="RL"]="LR"

  }

  #change Observer to uppercase
  if("Observer" %in% fix){

    data$Observer=toupper(data$Observer)

  }


  if("Swan" %in% fix){

    swan.probs=SwanCheck(data)

    for(i in swan.probs$missing.index){

      new.row=data[i,]

      if(new.row$Num==1){new.row$Obs_Type="single"}
      if(new.row$Num==2){new.row$Obs_Type="pair"}
      new.row$Species="SWAN"
      new.row$Num=1

      data=rbind(data, new.row)

    }

    for(j in swan.probs$bad.index){
      data$Obs_Type[j]="open"

      if(data$Year > 1998 || area != "WBPHS"){
      data$Num[j]=1
        }

    }

  }

  if("Obs_Type" %in% fix){


    for(k in 1:length(data$Obs_Type)){

      if(data$Species[k] %in% c("TUNE", "TRNE", "SWANN", "SWNE", "SW.N", "TS.N", "TSNE", "tune", "TUSWN"))
      {
        data$Num[k]=1
        data$Obs_Type[k]="open"
        next
        }

      if(data$Num[k]==1){

        data$Obs_Type[k]="single"

      }


      if(data$Num[k]==2){

        data$Obs_Type[k]="pair"
        data$Num[k]=1

      }

      if(data$Num[k] > 2){

        data$Obs_Type[k]="open"

      }

    }


  }

  if("Species" %in% fix){

  #data$OldSpecies=data$Species

    if(area!="WBPHS"){

  for(n in 1:length(data$Species)){

    data$Species[n]=sppntable$QAQC[sppntable$INPUT==data$Species[n]]

  }
    }


    if(area == "WBPHS"){

      for(n in 1:length(data$Species)){

        data$Species[n]=WBPHSsppntable$QAQC[WBPHSsppntable$INPUT==data$Species[n]]

      }
    }


    }


  return(data)

}

#' Check if common fixes need to be applied to a raw data file
#'
#' Raw2Analysis checks to see if any of the common fixes need to be applied to a data set.
#'
#' Raw2Analysis is a QA/QC check in GreenLight that will check for and apply common fixes to a data set through CommonFix.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The data frame to be checked.
#' @param area The project area designation.
#'
#' @return list of data and fix categories that should be applied
#'
#' @export
Raw2Analysis=function(data, area){
  fix=c("Species")

  if(length(unique(data$Obs_Type[!is.na(data$Obs_Type)])) <= 1){fix=c(fix, "Obs_Type")}

  seat.test=SeatCheck(data)
  if(seat.test$fail==TRUE){fix=c(fix, "Seat")}

  obs.test=ObserverCheck(data)
  if(obs.test==FALSE){fix=c(fix, "Observer")}

  swan.test=SwanCheck(data)
  if(swan.test$fail==TRUE){fix=c(fix, "Swan")}

  data=CommonFix(data=data, fix=fix, area=area)

  return(list("newdata"=data, "fix"=fix))

}


