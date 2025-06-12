#' Plot design strata estimates vs analysis strata estimates
#'
#' PlotChanges will take the design estimates and analysis strata estimates and plot them
#'
#' PlotChanges will take the design estimates and analysis strata estimates and plot them
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @return A ggplot object of design strata vs analysis strata estimates
#'
#' @export
PlotChanges = function(survey, species, index="total"){

  data=c()

  if(survey=="YKD"){d1 = YKDHistoric$combined
                    d2 = YKD$combined}
  if(survey=="YKG"){d1 = YKGHistoric$combined
  d2 = YKG$combined}
  if(survey=="ACP"){d1 = ACPHistoric$combined
  d2 = ACP$combined}
  if(survey=="CRD"){d1 = CRDHistoric$combined
  d2 = CRD$combined}


  if(index=="total"){

    old = d1 %>% filter(Species==species) %>% select(Year, total, total.se) %>% mutate(est="Analysis Strata")
    new = d2 %>% filter(Species==species) %>% select(Year, total, total.se) %>% mutate(est="Design Strata")
    data=rbind(old, new) %>%
      mutate(Species=species)

    colnames(data)[2]="index"
    colnames(data)[3]="se"
  }


  if(index=="itotal"){

    old = d1 %>% filter(Species==species) %>% select(Year, itotal, itotal.se) %>% mutate(est="Analysis Strata")
    new = d2 %>% filter(Species==species) %>% select(Year, itotal, itotal.se) %>% mutate(est="Design Strata")
    data=rbind(old, new) %>%
      mutate(Species=species)

    colnames(data)[2]="index"
    colnames(data)[3]="se"
  }

  if(index=="ibb"){

    old = d1 %>% filter(Species==species) %>% select(Year, ibb, ibb.se) %>% mutate(est="Analysis Strata")
    new = d2 %>% filter(Species==species) %>% select(Year, ibb, ibb.se) %>% mutate(est="Design Strata")
    data=rbind(old, new) %>%
      mutate(Species=species)

    colnames(data)[2]="index"
    colnames(data)[3]="se"
  }

  a.plot = ggplot(data=data, aes(x=Year, y=index, color=est)) +
    geom_point(position=position_dodge(width=0.5)) +
    ggtitle(paste(index, " ", species, " Population Index (95% CI)", sep="")) +
    geom_pointrange(aes(x=Year, y=index, ymin=pmax(0,index-1.96*se), ymax=index+1.96*se),data = data, position=position_dodge(width=0.5)) +
    theme(legend.title = element_blank())

  return(a.plot)

}
