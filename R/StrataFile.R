#' Create or append to a csv file with study area stratum information
#'
#' StrataFile will summarize study area stratum-level information into a csv file
#'
#' StrataFile will take a spatial layer with study area stratification polygons and
#' summarize the areas into a csv file.  The file can either be created entirely or appended
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
StrataFile <- function(
                        method = "create",
                        append.to = NULL){

if(method=="create"){

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
      strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/YKD/data/source_data/YK_Strata.gpkg"
    }

    if(area == "YKG"){
      year.panel = data.frame(
        year = c(1985:2019,2021:2025),
        panel = c(1985, 1985, 1985, 1985,
                  1989, 1989, 1989, 1989,
                  1993, 1993, 1993, 1993, 1993,
                  1998, rep(c("D","A","B","C"),6),"D", "A")
      )
      strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/YKD/data/source_data/YK_Strata.gpkg"

    }

    if(area == "ACP"){
      year.panel = data.frame(
        year = c(2007:2019,2022:2025),
        panel = c(rep(c("D","A","B","C"),4),"D")
      )
      strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/ACP/data/source_data/ACP_DesignStrata.gpkg"

    }

    if(area == "CRD"){
      year.panel = data.frame(
        year = c(1986:2012, 2014:2019, 2021:2025),
        panel = c(1986, 1987,
                  1988, 1988, 1988, 1988, 1988, 1988, 1988,
                  1995,
                  rep(1996, 17), rep("A", 11))
      )
      strata.path = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/CRD/data/source_data/CRD_DesignStrata.gpkg"

    }

    for (i in 1:length(year.panel$year)){


      print(paste(area, " ", year.panel$year[i]))

      entries=MasterFileList[MasterFileList$AREA==area & MasterFileList$YEAR == year.panel$year[i],]

      layer = entries$STRATA_LAYER[1]

      info = StrataSummarySF(strata.file=strata.path, id="STRATNAME", this.layer=layer)

      info$Survey=area

      info$Year = year.panel$year[i]

      full_file=rbind(full_file, info)

    }

}


MasterStrata = full_file

MasterStrata$layer.area = units::drop_units(MasterStrata$layer.area)

write.csv(MasterStrata, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterStrata.csv", quote = FALSE, row.names = FALSE)

save(MasterStrata, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterStrata.rda")

}

}





