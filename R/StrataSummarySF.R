
#' Summarize design strata for analysis using sf package
#'
#' StrataSummarySF will provide the overall study area spatial characteristics for analysis
#'
#' StratasummarySF will compute areas of each strata (in km^2) in a design strata file as well as calculate the maximum possible transects in
#' a sample (M).
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param strata.file The path to the design stratification file
#' @param id The identifier to group strata by
#'
#' @return data frame summary of stratification
#'
#' @export
StrataSummarySF=function(strata.file, id="STRATNAME", this.layer=NA){

  if(!(is.na(this.layer))){
  map = sf::st_read(dsn=strata.file, layer=this.layer, quiet=TRUE) %>%
    sf::st_transform(4269)}

  if(is.na(this.layer)){
    map = sf::st_read(dsn=strata.file, quiet=TRUE) %>%
      sf::st_transform(4269)}


  areas = map %>%
    dplyr::mutate(area=units::set_units(sf::st_area(map), km^2)) %>%
    dplyr::group_by(map[[id]]) %>%
    dplyr::summarise(STRATAREA=sum(area))

  colnames(areas)[1]="strata"

  bound = map %>%
    sf::st_bbox()

  how.many = ((bound$ymax - bound$ymin) * 111.5) / 0.2

  #200 meter spacing over 111.5 km per degree of lat
  spacing = .2/111.5

  newtrans = data.frame(y1=seq(bound$ymin, bound$ymax, by=spacing), x1=bound$xmin, y2=seq(bound$ymin, bound$ymax, by=spacing), x2=bound$xmax) %>%
    select(x1,y1,x2,y2)

  ls <- apply(newtrans, 1, function(x)
  {
    v <- as.numeric(x[c(1,3,2,4)])
    m <- matrix(v, nrow = 2)
    return(sf::st_sfc(sf::st_linestring(m), crs = 4269))
  })
  ls = Reduce(c, ls)

  ls=sf::st_sf(ls)

  ls$lineid=c(1:length(ls[[1]]))


  new.ls=sf::st_intersection(map, ls)

  possible = new.ls %>%
    dplyr::group_by(new.ls[[id]]) %>%
    dplyr::summarise(M = length(unique(lineid)))

  colnames(possible)[1]="strata"

  areasummary = data.frame("strata"=areas$strata, "layer.area"=areas$STRATAREA)

  areasummary = dplyr::left_join(areasummary,
              possible %>% dplyr::select(strata, M),
              by = "strata") %>%
    dplyr::select(strata, layer.area, M)

  units::set_units(areasummary$layer.area, km^2)

  if("Egg Island" %in% areasummary$strata | "egg" %in% areasummary$strata){

    areasummary$M[areasummary$strata=="Egg Island" | areasummary$strata=="egg"]=50


  }


 return(areasummary)

}

