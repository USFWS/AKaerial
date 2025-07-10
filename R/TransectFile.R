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


  for (i in 1:length(year.panel$year)){


    print(paste(area, " ", year.panel$year[i]))

    entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR == year.panel$year[i],]

    strata.path=paste(entries$DRIVE[1], entries$STRATA[1], sep="")
    strata.layer=entries$STRATA_LAYER[1]

    transect.path=paste(entries$DRIVE[1], entries$TRANS[1], sep="")

    if(!file.exists(strata.path)){next}
    if(!file.exists(transect.path)){next}

    layer.path = entries$LAYER[1]


    design=TransSummarySF(transect.file=transect.path,
                          transect.layer=layer.path,
                          strata.file=strata.path,
                          strata.layer=strata.layer,
                          strata.id="STRATNAME",
                          trans.id="OBJECTID")

    design$survey=entries$AREA[1]
    design$year=entries$YEAR[1]
    design$panel=year.panel$panel[i]

    full_file=rbind(full_file, design)

  }

}

  MasterTransect = full_file %>% sf::st_drop_geometry() %>% select(-Shape)

  MasterTransect$SampledArea = units::drop_units(MasterTransect$SampledArea)

  MasterTransect$LENGTH = units::drop_units(MasterTransect$LENGTH)

  colnames(MasterTransect)= c("Strata", "Transect", "Length",
                              "SampledArea", "ctran", "Survey", "Year", "Panel" )

  write.csv(MasterTransect, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterTransect.csv", quote = FALSE, row.names = FALSE)

  save(MasterTransect, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterTransect.rda")

}

