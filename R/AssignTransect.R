#' Assign transect ID to data for analysis using sf package
#'
#' AssignTransect will provide the closest transect associated with a point in a clean data set
#'
#' AssignTransect will calculate the nearest design transect to observations in a data file and overwrite the
#' Transect column in the data with the nearest LineString trans.id
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param transect.path The path to the design transect geopackage
#' @param trans.id The identifier of the naming column in the design file
#' @param transect.layer The layer referenced in the geopackage
#' @param full.data The data set of georeferenced observations
#'
#' @return data frame of observations with modified Transect column
#'
#' @export
AssignTransect=function(full.data, transect.path, trans.id, transect.layer){

  data.sf = sf::st_as_sf(full.data, coords=c("Lon", "Lat")) %>%
    st_set_crs(4326)

  design.trans = sf::st_read(transect.path, layer=transect.layer, quiet=TRUE)

  design.trans=sf::st_transform(design.trans, 4269)

  data.sf=sf::st_transform(data.sf, 4269)

  data.sf$closest=NA

  for(i in seq_len(nrow(data.sf))){

    data.sf$closest[i] = unlist(sf::st_drop_geometry(design.trans[
      sf::st_nearest_feature(data.sf[i,], design.trans),trans.id]))

  }

 final.data=sf::st_drop_geometry(data.sf)

  return(final.data)



}
