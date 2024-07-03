


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

    resample=data.frame("keep"=unique(data.select$design$OBJECTID)) %>%
      dplyr::arrange(keep) %>%
      dplyr::filter(dplyr::row_number() %% n == seed)

    trans = data.select$design %>%
      dplyr::filter(OBJECTID %in% resample$keep) %>%
      dplyr::filter(units::drop_units(LENGTH) > 0.01)


    #data.select$design = data.select$design[data.select$design$SPLIT %in% trans$SPLIT,]

    data.select$design = trans

    data.select$obs = data.select$obs %>% dplyr::filter(ctran %in% data.select$design$ctran)

    data.select$flight = data.select$flight %>% dplyr::filter(ctran %in% data.select$design$ctran)

    data.select$transect = data.select$transect %>% dplyr::filter(ctran %in% data.select$design$ctran)



    show = plot.data  %>% ggplot2::ggplot(ggplot2::aes(colour=STRATNAME)) +
      ggplot2::geom_sf(size=1.5) +
      ggplot2::geom_sf(data=sf::st_as_sf(data.select$design), colour="black", size=1.5)

    if(plot=="on"){print(show)}

  }

return(data.select)

}

