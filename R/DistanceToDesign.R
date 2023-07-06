#' Distance to the reported design transect
#'
#' DistanceToDesign will provide the shortest distance to the reported design transect associated with a point in a clean data set
#'
#' DistanceToDesign will calculate the distance to the nearest design transect
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param transect.path The acceptable distance before removal of the point in km
#' @param design.trans An sf object of the design transects
#' @param full.data The data set of georeferenced observations
#' @param trans.id The transect column reflecting the design numbering
#'
#' @return data frame of observations with modified Distance column
#'
#' @export
DistanceToDesign=function(full.data, design.trans, trans.id){

  data.sf = sf::st_as_sf(full.data, coords=c("Lon", "Lat")) %>%
    sf::st_set_crs(4326)

  design.trans=sf::st_transform(design.trans, 4269)

  data.sf=sf::st_transform(data.sf, 4269)

  sub=design.trans %>% dplyr::select(trans.id)

  for(i in seq_len(nrow(data.sf))){

    match = sub %>% filter(get(trans.id)==data.sf$DesignTransect[i])

    data.sf$Distance[i] = min(sf::st_distance(data.sf[i,], match))

  }

  final.data=data.sf

  return(final.data)



}
