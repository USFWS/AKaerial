

#' QA/QC checks for standardized aerial survey data
#'
#' GreenLightScribe will take input data, check for errors, create a summary report, and optionally output a new data file.
#'
#' GreenLightScribe is designed to automate common data QA/QC functions that are normally done live by an observer.
#' These checks run on raw Scribe data starting in 2023.
#' In the future, these checks will use a specific set of fields derived from a generic aerial survey protocol, but
#' this protocol does not currently exist, so instead the checks provided here are simply to streamline the
#' creation of population estimates using this R package and provide some consistency in the data fields before
#' a data set is shared with others.  The specific tests performed are:
#' \enumerate{
#' \item Column Names - Are all required columns present and named correctly?  Necessary columns are:
#'   \describe{
#'   \item{Species}{4 character string representing the acceptable species code for an observation.  See \code{\link{sppntable}}}
#'   \item{Count}{up to 5 digit integer representing the number seen}
#'   \item{Grouping}{character string representing observation type: \enumerate{
#'   \item single - one lone drake (dimorphic species) or lone bird (monomorphic species)
#'   \item pair - hen and drake in close association (dimorphic species) or 2 birds in close association (monomorphic species)
#'   \item open - a mixed sex flock that can't be classified as single, pair, or flkdrake
#'   \item flkdrake - 2 or more drakes in close association}}
#'   \item{Stratum}{character string (or numeric, treated as string) representing the stratum the observation was in (if known by the observer)}
#'   \item{Transect}{character string (or numeric, treated as string) of the DESIGN FILE transect number}
#'   \item{Segment}{character string (or numeric, treated as string) of the transect segment (if known)}
#'   \item{A_G_Name}{character string of air to ground segment ID}
#'   \item{Wind_Dir}{character string of wind direction based on 8 point cardinal/intercardinal directions}
#'   \item{Wind_Vel}{integer representing wind speed in knots}
#'   \item{Sky}{character string representing sky condition (clear, scattered, etc.)}
#'   \item{Behavior}{character string representing observed behavior: diving, flying, swimming, or NA}
#'   \item{Code}{character string representing the use of the data in analysis: \enumerate{
#'   \item use in standard index estimate
#'   \item use as double observer only
#'   \item additional data collected but not used in analysis}}
#'   \item{Notes}{character string reserved for additional comments}
#'   \item{Course}{character string of flight direction (numeric as degrees, character cardinal or intercardinal directions)}
#'   \item{Distance}{floating decimal representing distance from the nearest transect}
#'   \item{Latitude}{floating decimal representing decimal degrees of latitude in WGS84 datum}
#'   \item{Longitude}{floating decimal representing decimal degrees of longitude in WGS84 datum}
#'   \item{Year}{4 digit integer representing the year of the observation}
#'   \item{Month}{2 digit integer representing the month of the observation}
#'   \item{Day}{1 or 2 digit integer representing the day of the observation}
#'   \item{Observer}{3 character initials of the observer (such as CJF, all capitalized, or C_F)}
#'   \item{Seat}{2 character representation of seat assignment; RF (right front), LF (left front), RR (right rear), or LR (left rear)}
#'   \item{Time}{character string representing verbose time stamp}
#'   \item{Altitude}{floating decimal representing plane altitude in feet}
#'   \item{Speed}{floating decimal representing plane speed in miles per hour (?)}
#'   \item{Audio File}{character string representing .wav file recording for the associated observation}
#'   \item{# Satellites}{2 digit integer representing the number of satellites (?)}
#'   \item{HDOP}{2 digit integer representing unknown (?)}
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
#' \item Numeric columns - The columns Count, Wind_Vel, Distance, Latitude, Longitude, Year, Month, Day, Altitude, Speed, # Satellites, and HDOP must contain only numeric values.
#' }
#'
#' A file is given a "red light" and deemed inappropriate for analysis if it fails checks on required columns, is in an undefined area,
#' incorrect Obs_Types detected, unknown seat code, unrecognized species codes, multiple observers per file, or any of the numeric columns contain non-numerics.
#'
#' A file is given a "yellow light" that indicates inconsistencies in the file, but with known treatments by the function, if any of several common mistakes occur.
#' These include incorrect swan transcription, reversed seat codes (FR for front right instead of RF), use of older species codes, or lower case observer initials.
#'
#' If a file sufficiently passes quality checks (receives a green light) and scribe2analysis = TRUE, a QCobs (archive quality) .csv file and associated report is produced 2 directories above the path.name specified.
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
#' @param scribe2analysis TRUE or FALSE, should the archive version of the raw data file be generated (if possible)?
#' @param archive.dir Path to the desired archive directory for the new data file.  Defaults to 3 directory levels above the input file to follow current MBM practices.
#'
#' @return None
#'
#' @examples
#'  GreenlightScribe(path.name = "C:/DATA/MyData.csv", area = "CRD", report = TRUE, scribe2analysis = FALSE)
#'
#' @export
GreenLightScribe=function(path.name, area, report=TRUE, scribe2analysis=FALSE, archive.dir = "default"){

  can.fix=TRUE

  necessary=c("Year",
              "Month",
              "Day",
              "Seat",
              "Observer",
              "Stratum",
              "Transect",
              "Segment",
              "Course",
              "Altitude",
              "Speed",
              "A_G.Name",
              "Wind_Dir",
              "Wind_Vel",
              "Sky",
              "Audio.File",
              "Latitude",
              "Longitude",
              "Time",
              "HDOP",
              "X..Satellites",
              "Species",
              "Count",
              "Grouping",
              "Behavior",
              "Distance",
              "DistanceBin",
              "Code",
              "Notes")


  type <- tools::file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE, stringsAsFactors = FALSE, na.strings=c("NA", "na", "N/A"))}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "WBPHS", "BLSC", "VIS")){

    stop("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, VIS, WBPHS.")

  }



  #are all of the required columns there (and named correctly)?
  test.colmatch=ColMatch(data, necessary=necessary)

  if(test.colmatch==TRUE){s.colmatch="green"}else{s.colmatch="red"}


  #are there any swan nests without associated records (missing) or swan nests recorded as
  #anything but open?

  test.swan=SwanCheck(data)

  if(test.swan$fail==TRUE){s.swan="red"}else{s.swan="green"}


  #are there only 4 possible unit types? (Obs_type = single, pair, open, flkdrake)

  test.unit=UnitCheckScribe(data)

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


  #The columns Count, Wind_Vel, Distance, Latitude, Longitude, Year, Month, Day, Altitude, Speed, # Satellites, and HDOP must contain only numeric values.

  test.year=ShouldBeNumeric(data$Year)
  if(test.year$fail==TRUE){s.year="red"}else{s.year="green"}

  test.month=ShouldBeNumeric(data$Month)
  if(test.month$fail==TRUE){s.month="red"}else{s.month="green"}

  test.day=ShouldBeNumeric(data$Day)
  if(test.day$fail==TRUE){s.day="red"}else{s.day="green"}

  test.wind=ShouldBeNumeric(data$Wind_Vel)
  if(test.wind$fail==TRUE){s.wind="red"}else{s.wind="green"}

  test.lat=ShouldBeNumeric(data$Latitude)
  if(test.lat$fail==TRUE){s.lat="red"}else{s.lat="green"}

  test.lon=ShouldBeNumeric(data$Longitude)
  if(test.lon$fail==TRUE){s.lon="red"}else{s.lon="green"}

  test.hdop=ShouldBeNumeric(data$HDOP)
  if(test.hdop$fail==TRUE){s.hdop="red"}else{s.hdop="green"}

  test.distance=ShouldBeNumeric(data$Distance)
  if(test.distance$fail==TRUE){s.distance="red"}else{s.distance="green"}

  test.count=ShouldBeNumeric(data$Count)
  if(test.count$fail==TRUE){s.count="red"}else{s.count="green"}

  test.alt=ShouldBeNumeric(data$Altitude)
  if(test.alt$fail==TRUE){s.alt="red"}else{s.alt="green"}

  test.speed=ShouldBeNumeric(data$Speed)
  if(test.speed$fail==TRUE){s.speed="red"}else{s.speed="green"}

  test.sat=ShouldBeNumeric(data$X..Satellites)
  if(test.sat$fail==TRUE){s.sat="red"}else{s.sat="green"}


  ## E Osnas mapping function

  if(area != "WBPHS"){

  sf.obs <- data %>% sf::st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
    sf::st_transform(crs=3338) %>%
    filter(Code == "1") #remove any non-survey or special observations

  sf.lines <- sf.obs %>%
    sf::st_transform(crs=4326) %>%
    group_split(Transect, Day) %>%
    purrr::map(points2line) %>%
    purrr::map_dfr(rbind)

  if(area=="ACP"){basemap="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data/design_files/Design_Strata/ACP_DesignStrata.gpkg"}
  if(area %in% c("YKD", "YKG", "YKDV")){basemap="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_001_YKD_Aerial_Survey/data/source_data/YKD_DesignStrata.gpkg"}
  if(area=="CRD"){basemap="//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_005_CRD_Aerial_Survey/data/CRD_DesignStrata.gpkg"}

  basemap <- sf::st_read(dsn=basemap) %>%
    sf::st_transform(crs=3338)

  sf.obs <- sf.obs %>%  mutate(Day=as.character(Day))

}

  ## End mapping, plot the object in markdown

  rmd.path=system.file("rmd/greenlightscribe.Rmd", package="AKaerial")

 if(report == TRUE){
 rmd.path=system.file("rmd/greenlightscribe.Rmd", package="AKaerial")

 rmarkdown::render(rmd.path, output_dir=dirname(path.name), output_file=paste(basename(tools::file_path_sans_ext(path.name)), "_QAQC_", Sys.Date(), ".html", sep=''))
 }

  if(any("red" %in% c(s.count, s.hdop, s.alt, s.speed, s.sat, s.distance, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))){
    can.fix=FALSE
    print("can't fix")
    print(c(s.count, s.hdop, s.alt, s.speed, s.sat, s.distance, s.lon, s.lat, s.wind, s.day, s.month, s.year, s.oneobserver, s.species, s.colmatch, s.unit))}

  if(can.fix==TRUE & scribe2analysis==TRUE){

    rmd.path=system.file("rmd/scribe2analysis.Rmd", package="AKaerial")

    data.obj=Scribe2Analysis(data, area=area)

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
      }else{write.path=paste(dirname(path.name), "/", proj, "_", yr, "_QCObs_", name, ".csv", sep='')}

    }


    data.new$Notes=gsub(",","", data.new$Notes)


    print(write.path)

    if(area=="WBPHS"){write.csv(data.new[,1:25], write.path, quote=FALSE, row.names=FALSE, na="")
    }else{write.csv(data.new[,1:25], write.path, quote=FALSE, row.names=FALSE) }

    if(area!="VIS" & archive.dir != "default"){rmarkdown::render(rmd.path, output_dir=archive.dir, output_file=paste(proj,"_", yr, "_QCLog_",name, ".html", sep=''))}
    if(area=="VIS" & archive.dir != "default"){rmarkdown::render(rmd.path, output_dir=archive.dir, output_file=paste(proj, "_", yr, "_", region, "_", aircraft, "_QCLog_", name, ".html", sep=''))}
    if(area!="VIS" & archive.dir == "default"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj,"_", yr, "_QCLog_",name, ".html", sep=''))}
    if(area=="VIS" & archive.dir == "default"){rmarkdown::render(rmd.path, output_dir=dirname(dirname(dirname(path.name))), output_file=paste(proj, "_", yr, "_", region, "_", aircraft, "_QCLog_", name, ".html", sep=''))}

    }


}








#' Check if common fixes need to be applied to a raw data file
#'
#' Scribe2Analysis checks to see if any of the common fixes need to be applied to a data set.
#'
#' Scribe2Analysis is a QA/QC check in GreenlightScribe that will check for and apply common fixes to a data set through CommonFix.
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
Scribe2Analysis=function(data, area){
  fix=c("Species")

  if(length(unique(data$Grouping[!is.na(data$Grouping)])) <= 1){fix=c(fix, "Grouping")}

  seat.test=SeatCheck(data)
  if(seat.test$fail==TRUE){fix=c(fix, "Seat")}

  obs.test=ObserverCheck(data)
  if(obs.test==FALSE){fix=c(fix, "Observer")}

  swan.test=SwanCheck(data)
  if(swan.test$fail==TRUE){fix=c(fix, "Swan")}

  data$Delay = NA

  data = data %>%
    select(-c(HDOP, X..Satellites, Distance, Altitude, Speed))

  data=CommonFixScribe(data=data, fix=fix, area=area)


  colnames(data)[colnames(data)=="Count"]="Num"
  colnames(data)[colnames(data)=="Grouping"]="Obs_Type"
  colnames(data)[colnames(data)=="DistanceBin"]="Distance"
  colnames(data)[colnames(data)=="Audio.File"]="Filename"
  colnames(data)[colnames(data)=="Latitude"]="Lat"
  colnames(data)[colnames(data)=="Longitude"]="Lon"
  colnames(data)[colnames(data)=="Course"]="Flight_Dir"
  colnames(data)[colnames(data)=="A_G.Name"]="A_G_Name"

  data = data %>%
    select(Year, Month, Day, Seat, Observer, Stratum, Transect, Segment,
           Flight_Dir, A_G_Name, Wind_Dir, Wind_Vel, Sky, Filename, Lat, Lon,
           Time, Delay, Species, Num, Obs_Type, Behavior, Distance, Code, Notes)

  data$Time = data$Time %>%
    str_sub(start=12, end=22)

  data$Time = lubridate::period_to_seconds(lubridate::hms(data$Time))

  #go from UTC-6 to UTC-9
  data$Time = data$Time - (3 * 60 * 60)



  return(list("newdata"=data, "fix"=fix))

}


#' Check if Grouping entries are all correct
#'
#' UnitCheckScribe checks to see if Grouping entries are correct values needed for analysis.
#'
#' UnitCheckScribe is a GreenLight QA/QC check performed on a data set.  If the Grouping entries are not single, pair, flkdrake, or open, it will return a "red" status.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param col.data The column to be checked.
#'
#' @return data frame of TRUE/FALSE and offending entries
#'
#' @export
UnitCheckScribe=function(data){

  units=c("single", "pair", "flkdrake", "open", NA)

  bad=data[which(!(data$Grouping %in% units)),]

  return(list("fail"=(length(bad[,1])!=0), "bad"=bad))


}





#' Apply "yellow light" changes to a SCRIBE data set
#'
#' CommonFixScribe is used in conjunction with GreenLightScribe to apply "no-loss" changes to a data set
#'
#' CommonfixScribe will take "yellow light" issues and apply a known treatment to fix offending issues. The list of fixes includes \itemize{
#'   \item Seat - changes lower to uppercase, flips the characters (FR, FL, RL to RF, LF, LR)
#'   \item Observer - changes lower to uppercase
#'   \item Swan - breaks up a nest-only observation into 2 observations
#'   \item Grouping - changes open 2 or open 1 SWAN to pair or single, changes SWANN to open 1
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
CommonFixScribe=function(data, fix, area){

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

      if(new.row$Count==1){new.row$Grouping="single"}
      if(new.row$Count==2){new.row$Grouping="pair"}
      new.row$Species="SWAN"
      new.row$Count=1

      data=rbind(data, new.row)

    }

    for(j in swan.probs$bad.index){
      data$Grouping[j]="open"

      if(data$Year > 1998 || area != "WBPHS"){
        data$Count[j]=1
      }

    }

  }

  if("Grouping" %in% fix){


    for(k in 1:length(data$Grouping)){

      if(data$Species[k] %in% c("TUNE", "TRNE", "SWANN", "SWNE", "SW.N", "TS.N", "TSNE", "tune", "TUSWN"))
      {
        data$Count[k]=1
        data$Grouping[k]="open"
        next
      }

      if(data$Count[k]==1){

        data$Grouping[k]="single"

      }


      if(data$Count[k]==2){

        data$Grouping[k]="pair"
        data$Count[k]=1

      }

      if(data$Count[k] > 2){

        data$Grouping[k]="open"

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







#' Merge the daily SCRIBE files into a single project record for an observer
#'
#' ScribeMerge takes an input string and matches file names within a folder, then combines them into a single .csv file.
#'
#' SCRIBE software creates daily observation records that are most useful compiled into a complete observer record for a survey session.
#' ScribeMerge takes an input string and matches file names within a folder, then combines them into a single .csv file.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param folder The folder path containing the files.
#' @param string The character string common to the files to be combined.
#' @param addWBPHS TRUE/FALSE to add columns to convert HQ format to AK format
#'
#' @return No return value, but output is a new combined csv file within the folder.
#'
#' @export
ScribeMerge=function(folder, string, addWBPHS=FALSE){

  current.dir = getwd()

  setwd(folder)

  area = stringr::str_sub(list.files(folder, pattern=string)[1], 1, 3)

  files = list.files(folder, pattern=string) %>%
    purrr::map_df(~readr::read_csv(., col_types = readr::cols(
      Code = readr::col_character()), na = c("", "NA", "na", "N/A", "Not Found")
    ))

  year = files$Year[1]

  if(addWBPHS==TRUE){
    files$Behavior=NA
    files$DistanceBin=NA
    files$Code=1
    files$Notes=NA
    files$Sky=NA
    files$Wind_Dir=NA
    files$Wind_Vel=NA
  }

  files$Observer=toupper(files$Observer)
  files$Seat=toupper(files$Seat)


  readr::write_csv(files, file=paste(area, "_", year, "_RawObs_", string, ".csv", sep=""))

  setwd(current.dir)

}

