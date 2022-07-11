


TransectSample <- function(data.select, sample, n, plot="on", seed=0){

  if(sample == "random"){

    plot.data=sf::st_as_sf(data.select$design)

    trans = sample(unique(data.select$design$SPLIT),n)

    data.select$design = data.select$design[data.select$design$SPLIT %in% trans,]

    data.select$obs = data.select$obs %>% dplyr::filter(ctran %in% trans)

    data.select$flight = data.select$flight %>% dplyr::filter(PartOf %in% trans)

    data.select$transect = data.select$transect %>% dplyr::filter(ctran %in% trans)



    show = plot.data  %>% ggplot2::ggplot(aes(colour=STRATNAME)) +
      ggplot2::geom_sf(size=1.5) +
      ggplot2::geom_sf(data=sf::st_as_sf(data.select$design), colour="black", size=1.5)

    print(show)

  }


  if(sample == "stratified"){

    plot.data=sf::st_as_sf(data.select$design)

    trans = dplyr::distinct(data.select$design@data,SPLIT, STRATNAME) %>% group_by(STRATNAME) %>% sample_n(n)

    data.select$design = data.select$design[data.select$design$SPLIT %in% trans$SPLIT,]

    data.select$obs = data.select$obs %>% dplyr::filter(ctran %in% trans$SPLIT)

    data.select$flight = data.select$flight %>% dplyr::filter(PartOf %in% trans$SPLIT)

    data.select$transect = data.select$transect %>% dplyr::filter(ctran %in% trans$SPLIT)



    show = plot.data  %>% ggplot2::ggplot(aes(colour=STRATNAME)) +
      ggplot2::geom_sf(size=1.5) +
      ggplot2::geom_sf(data=sf::st_as_sf(data.select$design), colour="black", size=1.5)

    print(show)

  }

  if(sample == "systematic"){

    plot.data=sf::st_as_sf(data.select$design)

    data.select$design@data$sort.by = data.select$design@data$mid.Lat

    for (i in 1:length(data.select$design@data$STRATNAME)){
    if(data.select$design@data$STRATNAME[i] %in% c("Egg Island", "Egg", "egg")){

      data.select$design@data$sort.by[i] = data.select$design@data$mid.Lon[i]
    }
    }


    trans = data.select$design@data %>%
      dplyr::filter(len > 0.01) %>%
      dplyr::distinct(., SPLIT, STRATNAME, .keep_all=TRUE) %>%
      dplyr::filter(len > 0.01) %>%
      dplyr::group_by(STRATNAME) %>%
      dplyr::arrange(sort.by, .by_group = TRUE) %>%
      dplyr::filter(dplyr::row_number() %% n == seed)


    data.select$design = data.select$design[data.select$design$SPLIT %in% trans$SPLIT,]

    data.select$obs = data.select$obs %>% dplyr::filter(ctran %in% trans$SPLIT)

    data.select$flight = data.select$flight %>% dplyr::filter(PartOf %in% trans$SPLIT)

    data.select$transect = data.select$transect %>% dplyr::filter(ctran %in% trans$SPLIT)



    show = plot.data  %>% ggplot2::ggplot(ggplot2::aes(colour=STRATNAME)) +
      ggplot2::geom_sf(size=1.5) +
      ggplot2::geom_sf(data=sf::st_as_sf(data.select$design), colour="black", size=1.5)

    if(plot=="on"){print(show)}

  }

return(data.select)

}

