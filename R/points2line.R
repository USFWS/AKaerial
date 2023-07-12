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
    mutate(Year = Year[1], Transect=Transect)
  return(linestring)
}








#' Display a linestring object from a series of transect observations
#'
#' Mappoints2line displays a points2line Linestring object.
#'
#' This function will connect observations into a linestring object to attempt to recreate minimum sampling effort,
#' then display it.
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
Mappoints2line <- function(area, year){

  if(area=="YKD" | area=="YKG"){area=c("YKD", "YKG")}

  files = MasterFileList %>%
    filter(AREA %in% area & YEAR==year)

  data= paste(files$DRIVE, files$OBS, sep="") %>%
    purrr::map_df(~readr::read_csv(., col_types = readr::cols(Flight_Dir = readr::col_character(),
                                                              Sky = readr::col_character(),
                                                              Wind_Dir = readr::col_character(),
                                                              Notes = readr::col_character(),
                                                              Transect = readr::col_character(),
                                                              Segment = readr::col_character())))


  sf.obs <- data %>% sf::st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
    sf::st_transform(crs=3338) %>%
    filter(Code == 1) #remove any non-survey or special observations

  sf.lines <- sf.obs %>%
    sf::st_transform(crs=4326) %>%
    group_split(Transect, Day) %>%
    purrr::map(points2line) %>%
    purrr::map_dfr(rbind)

  if(area=="ACP"){basemap="K:/Waterfowl/ACP_Survey/Design_Files/Design_Strata/ACP_2007to2018_DesignStrata.shp"}
  if(area %in% c("YKD", "YKG", "YKDV")){basemap="K:/Waterfowl/YKD_Coastal/Design_Files/Design_Strata/YK_DesignStrata.shp"}
  if(area=="CRD"){basemap="K:/Waterfowl/CRD_Survey/Design_Files/Design_Strata/CRD_2018_DesignStrata.shp"}

  basemap <- sf::st_read(dsn=basemap) %>%
    sf::st_transform(crs=3338)

  sf.obs <- sf.obs %>%  mutate(Day=as.character(Day))

  tm <- tmap::tm_shape(basemap) + tmap::tm_polygons(col = "STRATNAME", alpha = 0.5) +
    tmap::tm_shape(sf.lines, name="Flown Track") + tmap::tm_lines() +
    tmap::tm_text("Transect", size=2) +
    tmap::tm_shape(sf.obs, name="Bird Obs") + tmap::tm_dots(col="Day") +
    tmap::tm_basemap(server = "Esri.WorldGrayCanvas") +
    tmap::tm_scale_bar()


  leaf = tmap::tmap_leaflet(tm)

  return(leaf)


}
