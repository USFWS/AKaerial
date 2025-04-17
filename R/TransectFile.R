#' Create or append to a csv file with study area transect panel information
#'
#' TransectFile will summarize study area transect-level information into a csv file
#'
#' TransectFile will take a spatial layer with study area stratification polygons and
#' transect line files and summarize the lengths into a csv file by year.  The file can either be
#' created entirely or appended to for a single year addition.
#' The file is intended to be an input into the estimate-generating functions in AKaerial.
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
TransectFile <- function(method="create",
                         output.folder="none",
                         append.to=NULL){

  full_file=c()

  for(j in 1:4){

  if(j==1){area="YKD"}
  if(j==2){area="YKG"}
  if(j==3){area="ACP"}
  if(j==4){area="CRD"}


  if(area == "YKD"){
    year.panel = data.frame(
      year = c(1988:2019,2021:2024),
      panel = c(1988:1998, rep(c("A","B","C","D"),6),"A")
    )
  strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/YKD/data/source_data/YKD_DesignStrata.gpkg"
  }

  if(area == "YKG"){
    year.panel = data.frame(
      year = c(1985:2019,2021:2024),
      panel = c(1985:1998, rep(c("A","B","C","D"),6),"A")
    )
    strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/YKD/data/source_data/YKD_DesignStrata.gpkg"

  }

  if(area == "ACP"){
    year.panel = data.frame(
      year = c(2007:2019,2022:2024),
      panel = c(2007:2009, rep(c("A","B","C","D"),3),"A")
    )
  }

  if(area == "CRD"){
    year.panel = data.frame(
      year = c(1986:2012, 2014:2019, 2021:2024),
      panel = c("A", "B", "C","D","E","F","G","H","I",rep("J", 18), rep("K", 10))
    )
  }


  for (i in 1:length(year.panel$year)){


    print(paste(area, " ", year.panel$year[i]))

    entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR == year.panel$year[i],]

    if(!(area %in% c("YKD", "YKG"))){strata.path=paste(entries$DRIVE[1], entries$STRATA[1], sep="")}

    transect.path=paste(entries$DRIVE[1], entries$TRANS[1], sep="")

    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    layer.path = entries$LAYER[1]


    design=TransSummarySF(transect.file=transect.path,
                          transect.layer=layer.path,
                          strata.file=strata.path,
                          strata.id="STRATNAME",
                          trans.id="OBJECTID")

    design$survey=entries$AREA[1]
    design$year=entries$YEAR[1]
    design$panel=year.panel$panel[i]

    full_file=rbind(full_file, design)

  }

}

  MasterTransect = full_file %>% sf::st_drop_geometry() %>% select(-Shape, -geom)

  MasterTransect$SampledArea = units::drop_units(MasterTransect$SampledArea)

  MasterTransect$LENGTH = units::drop_units(MasterTransect$LENGTH)

  colnames(MasterTransect)= c("Strata", "Transect", "Length",
                              "SampledArea", "ctran", "Survey", "Year", "Panel" )

  write.csv(MasterTransect, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterTransect.csv", quote = FALSE, row.names = FALSE)

  save(MasterTransect, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterTransect.rda")

}

