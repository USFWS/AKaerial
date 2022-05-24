#' Create a Linestring object from a series of transect observations
#'
#' points2line connects georeferenced points into an sf Linestring object.
#'
#' This function will connect observations into a linestring object to attempt to recreate minimum sampling effort.
#'
#' @author Erik Osnas, \email{erik_osnas@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param obs sf object of bird observations with referenced Lat Lon
#' @param Year year of the observations
#' @param Transect transect number the observations were associated with
#'
#' @return sf Linestring object
#'
#' @export
points2line <- function(obs, Year=unique(obs$Year), Transect=unique(obs$Transect), crs=4326){
  #this function accepts sf object of points and returns an sf linestring
  # sf object define ONE linestring, not more than one!
  #accepted sf object should have an attribute for named Year and Transect,
  # if not, supply as a parameter value

  print(Transect)

  linestring <- obs %>% cbind(sf::st_coordinates(.)) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    arrange(X, Y) %>%
    select(X, Y) %>%
    as.matrix() %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs=4326) %>%
    sf::st_sf(geometry=.) %>%
    mutate(Year = Year, Transect=Transect)
  return(linestring)
}

