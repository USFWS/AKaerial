
#' Apply \code{\link{Greenlight}} function to an entire folder
#'
#' CombinedReport will scan a folder for .csv data files and apply the \code{\link{Greenlight}}
#' function to each.
#'
#' CombinedReport is designed to produce QAQC reports for a large number of files based on a set of established criteria for data integrity.
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
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param method
#'
#' @return None
#'
#' @examples
#'  MassReport(area="ACP", method="both")
#'
#' @export
CombinedReport=function(strata.path=NA,
                        transect.path=NA,
                        data1.path=NA,
                        data2.path=NA,
                        area=NA,
                        threshold=.5){


  split.design=SplitDesign(strata.file = strata.path, transect.file = transect.path, SegCheck = FALSE)

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  split.design <- sp::spTransform(split.design, "+proj=longlat +ellps=WGS84")
  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  strata.names=unique(as.character(strata.proj$STRATNAME))


  boxes=BoundingByStrata(strata.path)

  data1=DataSelect(area=area, data.path=data1.path, strata.path=strata.path, transect.path=transect.path, threshold=threshold)
  data2=DataSelect(area=area, data.path=data2.path, strata.path=strata.path, transect.path=transect.path, threshold=threshold)

  total.obs=rbind(data1$obs, data2$obs)

  sp::coordinates(total.obs)=~Lon+Lat


  obs1.est=Densities(data1, area=area)
  obs2.est=Densities(data2, area=area)


  combine.est=CombineEstimates(rbind(obs1.est$estimates, obs2.est$estimate))
  est.by.strata=CombineByStrata(obs1.est$counts.final, obs2.est$counts.final)

  rmd.path=system.file("rmd/CombinedSurveyReport.Rmd", package="AKaerial")

  rmarkdown::render(rmd.path, output_dir=dirname(data1.path), output_file=paste(area,"_", data1$obs$Year[1], "_",data1$obs$Observer[1], "_", data2$obs$Observer[2], ".html", sep=''))


  }



BoundingByStrata= function(strata.path){

  #read projected (non-dataframe) strata
  strata.proj=LoadMap(strata.path, type="proj")

  strata.proj <- suppressWarnings(rgeos::gBuffer(strata.proj, byid=TRUE, width=0))


  strata.names=unique(as.character(strata.proj$STRATNAME))

  boxes = vector(mode = "list", length = length(strata.names))

  for(i in 1:length(strata.names)){

    boxes[[i]]$STRATNAME=strata.names[i]

    boxes[[i]]$coords=as.vector(sf::st_bbox(strata.proj[as.character(strata.proj$STRATNAME)==strata.names[i],]))


  }

  return(boxes)

}
