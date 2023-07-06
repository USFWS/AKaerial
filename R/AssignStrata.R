#' Assign strata ID to data for analysis using sf package
#'
#' AssignStrata will provide the strata layer associated with a point in a clean data set
#'
#' AssignStrata will calculate the nearest polygon to observations in a data file and overwrite the
#' Strata column in the data with the nearest polygon's strata.id
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param strata.file The path to the analysis stratification layer
#' @param strata.id The identifier of the stratification layer
#' @param full.data The data set of georeferenced observations
#' @param area The survey being processed
#'
#' @return data frame of observations with modified Strata column
#'
#' @export
AssignStrata=function(full.data, strata.file, strata.id, retain="liberal"){

  data.sf = sf::st_as_sf(full.data, coords=c("Lon", "Lat")) %>%
    st_set_crs(4326)

  analysis.strata = sf::st_read(dsn=strata.file, quiet=TRUE)

  analysis.strata=sf::st_transform(analysis.strata, 4269)

  data.sf=sf::st_transform(data.sf, 4269)

  out.list=sapply(sf::st_intersects(data.sf, analysis.strata), function(x){length(x)==0})

  out.of.area = data.sf[out.list,]
  out.of.area$Notes = "Retained"

  for(i in seq_len(nrow(out.of.area))){

    out.of.area$Stratum[i] = unlist(sf::st_drop_geometry(analysis.strata[
      sf::st_nearest_feature(out.of.area[i,], analysis.strata),strata.id]))

    }

  in.list=sapply(sf::st_intersects(data.sf, analysis.strata), function(x){length(x)!=0})

  in.area=data.sf[in.list,]

  intersect.list=sf::st_intersects(in.area, analysis.strata)

  for(j in seq_len(nrow(in.area))){

    in.area$Stratum[j] = unlist(sf::st_drop_geometry(analysis.strata[
      intersect.list[[j]],strata.id]))

  }

  in.area= in.area %>%
    dplyr::mutate(Lon= sf::st_coordinates(.)[,1],
                  Lat= sf::st_coordinates(.)[,2])

  out.of.area= out.of.area %>%
    dplyr::mutate(Lon= sf::st_coordinates(.)[,1],
                  Lat= sf::st_coordinates(.)[,2])

  in.area$Notes = as.character(in.area$Notes)
  out.of.area$Notes = as.character(out.of.area$Notes)

 if(retain=="liberal") {final.data=dplyr::bind_rows(in.area, out.of.area)}
 if(retain=="strict") {final.data=in.area}

  return(final.data)



}
