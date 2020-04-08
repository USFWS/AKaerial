
#' Runs a new combination of separate and combined estimates for 2 observers
#'
#' CombinedReport will produce an annual estimate report in .html format for 2 observers
#'
#' CombinedReport is designed to produce a basic .html report for quality checks for a particular year/crew combination. The report
#' contains basic visual QA/QC maps of stratification and transect design, as well as observation information by observer. It also
#' presents the basic and expanded table of estimates.  It uses the markdown script within AKaerial (CombinedSurveyReport.Rmd) and writes an
#' output report to the directory of data1.path.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The file location of the analysis stratification .shp file
#' @param transect.path The file location of the transect design .shp file
#' @param data1.path The file location of the .csv transcribed data for observer 1
#' @param data2.path The file location of the .csv transcribed data for observer 2
#' @param area The area code for dedicated MBM Alaska region surveys.  Either folder.path or area must be specified.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, for either geese or ducks
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param threshold The distance in kilometers from a design file where observations are no longer counted.  Defaults to 0.5 km.
#'
#' @return None
#'
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



#' Calculate bounding boxes for each distinct stratum in a stratification .shp file
#'
#' BoundingByStrata calculates a bounding box for each stratum in a .shp file in a format leaflet can use to display in markdown efficiently
#'
#' BoundingByStrata will calculate separate bounding boxes for each stratum in a .shp file.  It is primarily used to display each
#' stratum specifically in leaflet used within Rmarkdown.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param strata.path The path to the stratification .shp file.
#'
#' @return List of distinct strata and their bounding boxes.
#'
#' @export
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
