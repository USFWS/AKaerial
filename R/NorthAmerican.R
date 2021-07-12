

#' Prepare WBPHS data for index estimate calculation
#'
#' Load and adjust counts for WBPHS data to prepare for index estimate calculation
#'
#' This function takes standard greenlight data for the WBPHS (Waterfowl Breeding Population Habitat Survey, or North American) and prepares it for
#' index estimate calculations.  The function calls Cut9 to remove observations in Stratum 9 by a pre-defined longitudinal gradient for swans,
#' cackling Canada geese, and greater white-fronted geese.  These observations are removed prior to index estimation since they are included in
#' other MBM coastal zone surveys (YKG).  This function also adjusts the counts for each of 4 indices:
#' \enumerate{
#' \item itotal - Indicated total.  Singles doubled, pairs doubled, opens added, flkdrake 1-4 doubled, flkdrake 5+ added.
#' \item ibb - Indicated breeding birds.  Singles doubled, pairs doubled, opens removed, flkdrake 1-4 doubled, flkdrake 5+ removed.
#' \item total - Total birds.  Singles added, pairs doubled, opens added, flkdrake added.
#' \item sing1pair2 - Singles and pairs.  Singles added, pairs doubled, opens removed, flkdrake removed.
#' }
#' In addition, due to inconsistencies in interpretation of the field protocol for data collection, open 1 and open 2 are changed to single 1
#' and pair 1, respectively, across the entire data set.
#'
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data An R data frame of observations in a given year of the survey
#'
#' @return The same data frame with additional index columns and modified counts, as well as modified Stratum 9 observations.
#'
#' @export
ReadWBPHS= function(full.data){

  full.data=full.data[full.data$Code==1,]

  full.data$Observer=paste(full.data$Observer, full.data$Seat, sep=".")

  full.data=Cut9(full.data)

  full.data$Obs_Type[full.data$Obs_Type=="open" & full.data$Num==1 & full.data$Species!="SWANN"]="single"

  for (i in 1:length(full.data$Obs_Type)){
  if(full.data$Obs_Type[i]=="open" & full.data$Num[i]==2){
    full.data$Obs_Type[i]="pair"
  full.data$Num[i]=1
  }
  }


  full.data=AdjustCounts(full.data)

  return(full.data)

}




#' Clip stratum 9 for overlap for 3 species
#'
#' Clip the data from stratum 9 for CCGO, GWFG, and SWAN observations past a longitudinal gradient
#'
#' This function removes observations in Stratum 9 by a pre-defined longitudinal gradient for swans,
#' cackling Canada geese, and greater white-fronted geese.  These observations are removed prior to index estimation since they are included in
#' other MBM coastal zone surveys (YKG).  The gradient is as follows, by transect number:
#'  \itemize{
#'   \item 12:  -163.559
#'   \item 13:  -163.738
#'   \item 14:  -164.388
#'   \item 15:  -164.130
#'   \item 16:  -164.440
#'   \item 17:  -164.995
#'   \item 18:  -164.938
#'   \item 19:  -164.810
#'   }
#' Observations for CCGO, GWFG, and SWAN are trimmed past the western edge of the gradient and remaining observations are now attributed to
#' stratum 99 for documentation and to avoid confusion with stratum 9.
#'
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data An R data frame of observations in a given year of the survey
#'
#' @return The same data frame with modified Stratum 9 observations.
#'
#' @export
Cut9= function(full.data){

  #full.data$keep=1

  full.data$trans.seg = paste(full.data$Transect, full.data$Segment, sep=".")

  if(full.data$Year[1] >= 1999){

    long.cut <- data.frame(cbind(rep(9,8),
                               seq(12,19,1),
                               c(-163.559,-163.738,-164.388,-164.130,-164.440,-164.995,-164.938,-164.810)))

  names(long.cut) <- c("strata","tran","lon")

  augment.data = full.data %>%
    filter(Species %in% c("CCGO", "GWFG", "SWAN")) %>%
    filter(Stratum == 9) %>%
    filter((Transect == long.cut$tran[1] & Lon > long.cut$lon[1]) |
             (Transect == long.cut$tran[2] & Lon > long.cut$lon[2]) |
             (Transect == long.cut$tran[3] & Lon > long.cut$lon[3]) |
             (Transect == long.cut$tran[4] & Lon > long.cut$lon[4]) |
             (Transect == long.cut$tran[5] & Lon > long.cut$lon[5]) |
             (Transect == long.cut$tran[6] & Lon > long.cut$lon[6]) |
             (Transect == long.cut$tran[7] & Lon > long.cut$lon[7]) |
             (Transect == long.cut$tran[8] & Lon > long.cut$lon[8]) |
             (!(Transect %in% long.cut$tran)))

  if(length(augment.data[,1])>0){augment.data$Stratum = 99}


  }


 if(full.data$Year[1] < 1999) {

  seg.cut <- data.frame("Transect"=c(13,14,16,17,17,18,18,19,19), "Segment"=c(11,11,8,5,6,5,6,5,6))

  seg.cut$trans.seg = paste(seg.cut$Transect, seg.cut$Segment, sep=".")

  augment.data = full.data %>%
    filter(Species %in% c("CCGO", "GWFG", "SWAN")) %>%
    filter(Stratum == 9) %>%
    filter(!(trans.seg %in% seg.cut$trans.seg))

    if(length(augment.data[,1])>0){augment.data$Stratum = 99}

 }


 full.data=rbind(full.data, augment.data)


  return(full.data)
}


#' Summarize the flight information for an observer on the WBPHS
#'
#' SummaryWBPHS will summarize the flight and sampled area of a pilot or observer and give both the original and renumbered transect for reference
#'
#' This function takes an observation file and uses it to imply the sampled area for each observer.  Segment, transect, and strata records are pulled
#' from the observations and compared to a table of known values for lengths and sizes.  Segments in all strata except stratum 7 and stratum 12
#' are 16 miles long.  Segments in stratum 7 are 8 miles long and segments in stratum 12 are 18 miles long.  Total lengths for the augmented transects 12-19
#' in modified stratum 9 (see Cut9) are:
#' \itemize{
#'   \item 12:  22 mi
#'   \item 13:  20.763 mi
#'   \item 14:  21.32 mi
#'   \item 15:  12 mi
#'   \item 16:  14.543 mi
#'   \item 17:  8.395 mi
#'   \item 18:  7.663 mi
#'   \item 19:  8.372 mi
#'   }
#'  Default strip width is 0.25 mi for 2 observers, each viewing 0.125 mi.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param full.data A clean (greenlight) file of observations
#' @param strip.width The sampled strip width in miles
#'
#' @return Data frame containing strata, transect, and segment information by observer
#'
#' @export
SummaryWBPHS=function(full.data, strip.width=0.25){

  flight=data.frame(

    Year=numeric(),
    Observer=character(),
    Stratum=character(),
    Transect=character(),
    n.segs=numeric(),
    Length=numeric(),
    SampledArea=numeric(), stringsAsFactors = FALSE
  )



  for (observer in unique(full.data$Observer)){

    temp.data=full.data[full.data$Observer==observer,]
    st.sets=(unique(temp.data[,c("Stratum", "Transect")]))
    st.sets$n.segs=0
    st.sets$Length=0


    for(i in 1:length(st.sets[,1])){

      st.sets$n.segs[i]=length(unique(temp.data$Segment[temp.data$Stratum==st.sets$Stratum[i] & temp.data$Transect==st.sets$Transect[i]]))

      seg.len=16

      if(st.sets$Stratum[i]==7){seg.len=8}
      if(st.sets$Stratum[i]==12){seg.len=18}

      st.sets$Length[i]=seg.len*st.sets$n.segs[i]

    }


    temp.flight=data.frame(

      Year=rep(temp.data$Year[1], length(st.sets[,1])),
      Observer=rep(temp.data$Observer[1], length(st.sets[,1])),
      Stratum=st.sets$Stratum,
      Transect=st.sets$Transect,
      n.segs=st.sets$n.segs,
      Length=st.sets$Length, stringsAsFactors = FALSE
    )


    temp.flight$SampledArea=temp.flight$Length*strip.width/2

    flight=rbind(flight, temp.flight)

  }


  for (i in 1:length(flight$Stratum)){

    if(flight$Stratum[i]==99 && flight$Year >= 1999){

      if(flight$Transect[i]==12){flight$SampledArea[i]=22}
      if(flight$Transect[i]==13){flight$SampledArea[i]=20.763}
      if(flight$Transect[i]==14){flight$SampledArea[i]=21.32}
      if(flight$Transect[i]==15){flight$SampledArea[i]=12}
      if(flight$Transect[i]==16){flight$SampledArea[i]=14.543}
      if(flight$Transect[i]==17){flight$SampledArea[i]=8.395}
      if(flight$Transect[i]==18){flight$SampledArea[i]=7.663}
      if(flight$Transect[i]==19){flight$SampledArea[i]=8.372}



    }

  }



  return(flight)
}


#' Summarize index counts for WBPHS data by Segment, Transect, and Stratum
#'
#' Summarize index counts for WBPHS data by Segment, Transect, and Stratum
#'
#' This function takes standard greenlight data for the WBPHS (Waterfowl Breeding Population Habitat Survey, or North American) and
#' summarizes it at the Segment, Transect, and Stratum level.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param selected.data An R data frame of observations in a given year of the survey
#'
#' @return A data frame with index values by Year, Observer, Species, Obs_Type, Segment, Transect, and Stratum.
#'
#' @export
TransDataWBPHS=function(selected.data){

    agg=aggregate(cbind(Num,itotal,total,ibb, sing1pair2)~Year+Observer+Species+Obs_Type+Stratum+Transect+Segment,data=selected.data, FUN=sum)


    colnames(agg)=c("Year", "Observer", "Species", "Obs_Type", "Stratum", "Transect", "Segment", "Num", "itotal", "total", "ibb", "sing1pair2")

    agg=as.data.frame(tidyr::complete(data=agg, Year, Observer, Species, Obs_Type, tidyr::nesting(Stratum, Transect, Segment), fill=list(Num=0, itotal=0, total=0, ibb=0, sing1pair2=0)))

    agg$area=0

    return(agg[order(agg$Year, agg$Observer, agg$Species, agg$Stratum, agg$Transect, agg$Segment, agg$Obs_Type),])

  }





#' Summarize index counts for WBPHS data by Transect and Stratum
#'
#' Summarize index counts for WBPHS data by Transect and Stratum
#'
#' This function takes standard greenlight data for the WBPHS (Waterfowl Breeding Population Habitat Survey, or North American) and
#' summarizes it at the Transect and Stratum levels. It uses already adjusted counts by Obs_Type (see AdjustCounts).
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param adj.counts An R data frame of observations in a given year of the survey
#'
#' @return A data frame with index values by Year, Observer, Species, Transect, and Stratum.
#'
#' @export
CountsWBPHS=function(adj.counts) {



  t1=(aggregate(adj.counts$total~adj.counts$Year+adj.counts$Observer+adj.counts$Transect+adj.counts$Species+adj.counts$Stratum, FUN=sum))
  t2=(aggregate(adj.counts$itotal~adj.counts$Year+adj.counts$Observer+adj.counts$Transect+adj.counts$Species+adj.counts$Stratum, FUN=sum))
  t2b=aggregate(adj.counts$ibb~adj.counts$Year+adj.counts$Observer+adj.counts$Transect+adj.counts$Species+adj.counts$Stratum, FUN=sum)
  t4=aggregate(adj.counts$sing1pair2~adj.counts$Year+adj.counts$Observer+adj.counts$Transect+adj.counts$Species+adj.counts$Stratum, FUN=sum)
  t3=merge(t1,t2,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$Transect", "adj.counts$Species", "adj.counts$Stratum"))
  t3=merge(t3,t2b,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$Transect", "adj.counts$Species", "adj.counts$Stratum"))
  t3=merge(t3,t4,by=c("adj.counts$Year", "adj.counts$Observer","adj.counts$Transect", "adj.counts$Species", "adj.counts$Stratum"))


  colnames(t3)=c("Year","Observer","Transect", "Species", "Stratum", "total", "itotal", "ibb", "sing1pair2")

  return(t3[order(t3$Year, t3$Observer, t3$Species, t3$Stratum, t3$Transect),])




}



#' Standard ratio estimator for WBPHS survey data
#'
#' EstimatesWBPHS will combine spatially-referenced observations with design transects and strata to create an index estimate
#'
#' EstimatesWBPHS is the primary function in AKaerial for producing index estimates for the WBPHS survey. It will take an object from ReadWBPHS and adjust counts, summarize
#' spatial information, and calculate indices and their associated standard errors.  Also retained are estimates of densities of bird by strata and on
#'  each transect.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The ReadWBPHS list object to be analyzed
#' @param flight Flight summary from SummaryWBPHS
#'
#' @return List object with 2 elements: \enumerate{
#' \item output.table Observer-specific estimates for the species indicated in the sppntable estimates column
#' \item expanded.table Deeper level count information by transect, strata, species, and observer
#' }
#'
#' @export
EstimatesWBPHS=function(data, flight){



  counts.t=CountsWBPHS(data)
  counts.t$area=0

  for (i in 1:length(counts.t$area)){

    counts.t$area[i]=flight$SampledArea[flight$Year==counts.t$Year[i] & flight$Observer==counts.t$Observer[i] & flight$Transect==counts.t$Transect[i] & flight$Strata==counts.t$Stratum[i]][1]

  }

  t3=counts.t
  #Remove transect start and end from species list
  t3=t3[t3$Species != "NoBirdsSeen",]
  t3=t3[t3$Species != "START",]
  t3=t3[t3$Species != "END",]


  sp.strat.total=aggregate(t3$total~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=sum)
  colnames(sp.strat.total)=c("Year","Observer", "Species", "Stratum", "total")

  sp.strat.itotal=aggregate(t3$itotal~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=sum)
  colnames(sp.strat.itotal)=c("Year","Observer", "Species", "Stratum", "itotal")

  sp.strat.ibb=aggregate(t3$ibb~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=sum)
  colnames(sp.strat.ibb)=c("Year","Observer", "Species", "Stratum", "ibb")

  sp.strat.sing1pair2=aggregate(t3$sing1pair2~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=sum)
  colnames(sp.strat.sing1pair2)=c("Year","Observer", "Species", "Stratum", "sing1pair2")

  #Variance of the counts within each Stratum
  sp.strat.total.v=aggregate(t3$total~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=var)
  colnames(sp.strat.total.v)=c("Year", "Observer", "Species", "Stratum", "total.v")

  sp.strat.itotal.v=aggregate(t3$itotal~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=var)
  colnames(sp.strat.itotal.v)=c("Year", "Observer", "Species", "Stratum", "itotal.v")

  sp.strat.ibb.v=aggregate(t3$ibb~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=var)
  colnames(sp.strat.ibb.v)=c("Year","Observer", "Species", "Stratum", "ibb.v")

  sp.strat.sing1pair2.v=aggregate(t3$sing1pair2~t3$Year+t3$Observer+t3$Species+t3$Stratum, FUN=var)
  colnames(sp.strat.sing1pair2.v)=c("Year","Observer", "Species", "Stratum", "sing1pair2.v")

  sp.strat=merge(sp.strat.total, sp.strat.itotal)
  sp.strat=merge(sp.strat, sp.strat.ibb)
  sp.strat=merge(sp.strat, sp.strat.sing1pair2)

  sp.strat.v=merge(sp.strat.total.v, sp.strat.itotal.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.ibb.v)
  sp.strat.v=merge(sp.strat.v, sp.strat.sing1pair2.v)

  #Put the totals together and leave placeholders for var and cov
  sp.strat.final=merge(sp.strat, sp.strat.v)
  sp.strat.final$total.cov=0
  sp.strat.final$itotal.cov=0
  sp.strat.final$var.N=0
  sp.strat.final$var.Ni=0
  sp.strat.final$ibb.cov=0
  sp.strat.final$var.Nib=0
  sp.strat.final$sing1pair2.cov=0
  sp.strat.final$var.Nsing1pair2=0
  sp.strat.final$ssq.total=0
  sp.strat.final$ssq.itotal=0
  sp.strat.final$ssq.ibb=0
  sp.strat.final$ssq.sing1pair2=0

  sp.strat.final$cxa.total=0
  sp.strat.final$cxa.itotal=0
  sp.strat.final$cxa.ibb=0
  sp.strat.final$cxa.sing1pair2=0

  sp.strat.final$mean.area=0

  for (i in 1:length(sp.strat.final$Stratum)){

    sp.strat.final$ssq.total[i]=sum(t3$total[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]]^2)
    sp.strat.final$ssq.itotal[i]=sum(t3$itotal[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]]^2)
    sp.strat.final$ssq.ibb[i]=sum(t3$ibb[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]]^2)
    sp.strat.final$ssq.sing1pair2[i]=sum(t3$sing1pair2[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]]^2)

    sp.strat.final$cxa.total[i]=sum(t3$total[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]] * t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]])
    sp.strat.final$cxa.itotal[i]=sum(t3$itotal[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]] * t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]])
    sp.strat.final$cxa.ibb[i]=sum(t3$ibb[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]] * t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]] )
    sp.strat.final$cxa.sing1pair2[i]=sum(t3$sing1pair2[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]] * t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]])

    sp.strat.final$ssq.area[i]=sum(t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]]^2)

    sp.strat.final$mean.area[i]=mean(t3$area[t3$Species==sp.strat.final$Species[i] & t3$Stratum==sp.strat.final$Stratum[i]])

  }





  #Calculate the total area by type and the variance of the areas

  area.strat=aggregate(t3$area~t3$Year+t3$Observer+t3$Stratum+t3$Species, FUN=sum)
  area.strat.v=aggregate(t3$area~t3$Year+t3$Observer+t3$Stratum+t3$Species, FUN=var)

  colnames(area.strat)=c("Year", "Observer", "Stratum", "Species","total.area")
  colnames(area.strat.v)=c("Year", "Observer", "Stratum", "Species", "total.area.var")

  area.strat=area.strat[!duplicated(area.strat[1:3]),-4]
  area.strat.v=area.strat.v[!duplicated(area.strat.v[1:3]),-4]


  #Put spatial summary together
  area.summary=merge(area.strat, area.strat.v)
  #print(area.summary)


  #Merge the counts and spatial stats
  counts.final=merge(sp.strat.final,area.summary, by=c("Year", "Observer", "Stratum"))

  #Calculate final densities for each strata layer
  density.total=counts.final$total/counts.final$total.area
  density.itotal=counts.final$itotal/counts.final$total.area
  density.ibb=counts.final$ibb/counts.final$total.area
  density.sing1pair2=counts.final$sing1pair2/counts.final$total.area


  counts.final=cbind(counts.final, density.total, density.itotal, density.ibb, density.sing1pair2)
  #print(head(counts.final))


  strata.area=data.frame("Stratum"=c(1, 2,3,4,5,6,7,8,9,99,10,11,12), "layer.area"=c(2200,3900,9300,10800,3400,4100,400,9900,26600,21637.937,3850,5350,1970))
  counts.final=merge(counts.final, strata.area, by="Stratum")


  #Extrapolate density estimates across area calculation
  total.est=counts.final$density.total * counts.final$layer.area
  itotal.est=counts.final$density.itotal * counts.final$layer.area
  ibbtotal.est=counts.final$density.ibb * counts.final$layer.area
  sing1pair2.est=counts.final$density.sing1pair2 * counts.final$layer.area

  counts.final=cbind(counts.final, total.est, itotal.est,ibbtotal.est, sing1pair2.est)


  #Summarize in table
  estimates=aggregate(counts.final$total.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates)=c("Year", "Observer", "Species", "total.est")

  estimates.i=aggregate(counts.final$itotal.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.i)=c("Year", "Observer", "Species","itotal.est")

  estimates.ibb=aggregate(counts.final$ibbtotal.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.ibb)=c("Year", "Observer", "Species","ibbtotal.est")

  estimates.sing1pair2=aggregate(counts.final$sing1pair2.est~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(estimates.sing1pair2)=c("Year", "Observer", "Species","sing1pair2.est")

  estimates=merge(estimates, estimates.i, by=c("Year", "Observer", "Species"))
  estimates=merge(estimates, estimates.ibb, by=c("Year", "Observer", "Species"))
  estimates=merge(estimates, estimates.sing1pair2, by=c("Year", "Observer", "Species"))

  #Strata level estimates organized





  diff.lat=area.summary

  diff.lat$m=diff.lat$total.area
  diff.lat$M=0

  for (i in 1:length(diff.lat$Stratum)){

    diff.lat$M[i]=strata.area$layer.area[strata.area$Stratum==diff.lat$Stratum[i]]/diff.lat$m[i]


  }

  #See equation 12.9, p. 249 in "Analysis and Management of Animal Populations"
  #Williams, Nichols, Conroy; 2002

  for (j in 1:length(counts.final$Species)){

    counts.final$var.N[j]=counts.final$layer.area[j]^2*(counts.final$ssq.total[j]-2*counts.final$density.total[j]*counts.final$cxa.total[j]+(counts.final$density.total[j]^2)*(counts.final$ssq.area[j]))/(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])*(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Ni[j]=counts.final$layer.area[j]^2*(counts.final$ssq.itotal[j]-2*counts.final$density.itotal[j]*counts.final$cxa.itotal[j]+(counts.final$density.itotal[j]^2)*(counts.final$ssq.area[j]))/(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])*(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Nib[j]=counts.final$layer.area[j]^2*(counts.final$ssq.ibb[j]-2*counts.final$density.ibb[j]*counts.final$cxa.ibb[j]+(counts.final$density.ibb[j]^2)*(counts.final$ssq.area[j]))/(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])*(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Nsing1pair2[j]=counts.final$layer.area[j]^2*(counts.final$ssq.sing1pair2[j]-2*counts.final$density.sing1pair2[j]*counts.final$cxa.sing1pair2[j]+(counts.final$density.sing1pair2[j]^2)*(counts.final$ssq.area[j]))/(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])*(length(t3$Transect[t3$Stratum==counts.final$Stratum[j] & t3$Species==counts.final$Species[j]])-1)*counts.final$mean.area[j]^2)

  }



  var.est=aggregate(counts.final$var.N~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est)=c("Year", "Observer", "Species","var.N")

  var.est.i=aggregate(counts.final$var.Ni~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.i)=c("Year", "Observer", "Species","var.Ni")

  var.est.ibb=aggregate(counts.final$var.Nib~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.ibb)=c("Year", "Observer", "Species","var.Nib")

  var.est.sing1pair2=aggregate(counts.final$var.Nsing1pair2~counts.final$Year+counts.final$Observer+counts.final$Species, FUN=sum)
  colnames(var.est.sing1pair2)=c("Year", "Observer", "Species","var.Nsing1pair2")



  estimates=merge(estimates, var.est, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.i, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.ibb, by=c("Year", "Observer", "Species"), all=TRUE)
  estimates=merge(estimates, var.est.sing1pair2, by=c("Year", "Observer", "Species"), all=TRUE)


  estimates$SE=sqrt(estimates$var.N)
  estimates$SE.i=sqrt(estimates$var.Ni)
  estimates$SE.ibb=sqrt(estimates$var.Nib)
  estimates$SE.sing1pair2=sqrt(estimates$var.Nsing1pair2)


  estimates$total.est=as.integer(estimates$total.est)
  estimates$itotal.est=as.integer(estimates$itotal.est)
  estimates$ibbtotal.est=as.integer(estimates$ibbtotal.est)
  estimates$sing1pair2.est=as.integer(estimates$sing1pair2.est)


  counts.final$SE=sqrt(counts.final$var.N)
  counts.final$SE.i=sqrt(counts.final$var.Ni)
  counts.final$SE.ibb=sqrt(counts.final$var.Nib)
  counts.final$SE.sing1pair2=sqrt(counts.final$var.Nsing1pair2)


  return(list("estimates"=estimates, "expanded"=counts.final[order(counts.final$Species, counts.final$Observer, counts.final$Stratum),]))


  #return(list("estimates"=estimates, "diff.lat"=diff.lat, "strata.area"=strata.area, "counts.final"=counts.final))

}



#' Combine and parse raw WBPHS data files
#'
#' SplitWBPHS will combine multiple observation files and split into annual observer files
#'
#' SplitWBPHS takes a folder of raw text files (generally at the transect or stratum level) and combines them into an annual data file
#' for an observer using the naming convention: WBPHS_YYYY_RawObs_NNN.csv, where YYYY is the 4 digit year and NNN is 3 character initials.
#' Columns must be in the following order: Year, Month, Day, Seat, Observer, Stratum, Transect, Segment, Flight_Dir, A_G_Name, Wind_Dir,
#' Wind_Vel, Sky, Filename, Lat, Lon, Time, Delay, Species, Num, Obs_Type.
#' It will also add the columns Behavior, Distance, Code, and Notes that aren't used in the standard WBPHS protocol, but have become
#' the standard in Alaska Region aerial surveys.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param folder.path The directory path to the folder for input and output files.
#'
#' @return Year-Observer combinations of data in csv format to folder.path
#'
#' @export
SplitWBPHS= function(folder.path){

  setwd(folder.path)

  full.data=data.frame(
    "Year"=numeric(),
    "Month"=numeric(),
    "Day"=numeric(),
    "Seat"=character(),
    "Observer"=character(),
    "Stratum"=character(),
    "Transect"=character(),
    "Segment"=character(),
    "Flight_Dir"=numeric(),
    "A_G_Name"=character(),
    "Wind_Dir"=character(),
    "Wind_Vel"=numeric(),
    "Sky"=character(),
    "Filename"=character(),
    "Lat"=numeric(),
    "Lon"=numeric(),
    "Time"=numeric(),
    "Delay"=numeric(),
    "Species"=character(),
    "Num"=numeric(),
    "Obs_Type"=character(), stringsAsFactors = FALSE)




  file_list <- list.files(folder.path)

  for (file in file_list){


    type <- file_ext(file)

    if(type == "docx" || type=="txt" || type=="csv") {
      print(paste("Skipped", file))
      next}


    print(paste("Included file", file, "successfully."))

    temp.data <-read.csv(file, header=FALSE)
    colnames(temp.data)=colnames(full.data)
    full.data<-rbind(full.data, temp.data)


  }

  full.data$Behavior=NA
  full.data$Distance=NA
  full.data$Code=1
  full.data$Notes=NA
  full.data$Observer=toupper(full.data$Observer)

  obs.list=unique(full.data$Observer)

 for (i in 1:length(obs.list)){

   temp.data=full.data[full.data$Observer==obs.list[i],]

   yr.list=unique(temp.data$Year)

   for (k in 1:length(yr.list)){

   write.csv(temp.data[temp.data$Year==yr.list[k],], paste(folder.path, "/WBPHS_", yr.list[k], "_RawObs_", obs.list[i], ".csv", sep=""),
             row.names=FALSE, quote=FALSE)

   }
 }


}



#' Combine WBPHS estimates by strata
#'
#' CombineEstimatesByStrata will combine multiple observer estimates at the strata level
#'
#' CombineEstimatesByStrata takes an estimate object (See EstimatesWBPHS) and merges the estimates of 2 observers by species at the strata level.
#' The resulting data frame is appended onto the master WBPHS results table.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param estimates WBPHS index estimates from EstimatesWBPHS.
#'
#' @return Data frame of combined index estimates by species at the strata level
#'
#' @export
CombineEstimatesByStrata=function(estimates){

  yr.list=unique(estimates$Year)
  sp.list=unique(estimates$Species)
  strata.list=unique(estimates$Stratum)

  combined=data.frame(Year=rep(yr.list, each=length(unique(estimates$Species))*length(strata.list)), Species=rep(sp.list, length(yr.list)*length(strata.list)), Stratum=rep(strata.list, each=length(yr.list)*length(sp.list)), total=0, total.var=0, total.se=0, itotal=0, itotal.var=0, itotal.se=0, ibb=0, ibb.var=0, ibb.se=0, sing1pair2=0, sing1pair2.var=0, sing1pair2.se=0)



  for(i in 1:length(combined$Year)){


    combined$total[i]=mean(estimates$total.est[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

    combined$itotal[i]=mean(estimates$itotal.est[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

    combined$ibb[i]=mean(estimates$ibbtotal.est[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])

    combined$sing1pair2[i]=mean(estimates$sing1pair2.est[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])


    combined$total.var[i]=sum(estimates$var.N[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.N[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

    combined$itotal.var[i]=sum(estimates$var.Ni[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Ni[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

    combined$ibb.var[i]=sum(estimates$var.Nib[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nib[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)

    combined$sing1pair2.var[i]=sum(estimates$var.Nsing1pair2[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])/(length(estimates$var.Nsing1pair2[estimates$Stratum==combined$Stratum[i] & estimates$Year==combined$Year[i] & estimates$Species==combined$Species[i]])^2)


  }

  combined$total.se=sqrt(combined$total.var)
  combined$itotal.se=sqrt(combined$itotal.var)
  combined$ibb.se=sqrt(combined$ibb.var)
  combined$sing1pair2.se=sqrt(combined$sing1pair2.var)



  return(combined[order(combined$Species,combined$Stratum),])

}



#' Display observations on WBPHS segments
#'
#' ShowWBPHS will load a leaflet display of WBPHS design segments and observations
#'
#' ShowWBPHS provides a quick spatial overview of a set of observations and the Waterfowl Breeding Population Habitat Survey segments.
#' I am unsure of the origin and accuracy of the segments.  Segments and observations are drawn using leaflet.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data WBPHS clean (greenlighted) observations.
#' @param trans.file Directory path to the WBPHS segment shape file (default to Q:/Waterfowl/WBPHS/Design Files/Design_Transects/NAWBPS_segments.shp)
#'
#' @return Interactive map of segment-level data
#'
#' @export
ShowWBPHS=function(data, trans.file="Q:/Waterfowl/WBPHS/Design Files/Design_Transects/NAWBPS_segments.shp"){

  trans.layer=file_path_sans_ext(basename(trans.file))

  trans.obj=readOGR(trans.file, trans.layer, verbose=FALSE)
  trans.proj <- spTransform(trans.obj, "+proj=longlat +ellps=WGS84")

  pal=colorFactor(palette = c("red", "green", "yellow", "blue", "orange"), trans.proj$Segment)
  pal2=colorFactor(palette = c("red", "yellow"), data$Observer)

map= leaflet() %>%
  addTiles() %>%

  addPolylines(data=trans.proj,
               color=~pal(Segment),
               weight=4,
               opacity=.9,
               label=~trans.proj$Seq,
               popup = paste("Strata: ", trans.proj$Seq, "<br>",
                             "Transect: ", trans.proj$Transect, "<br>",
                             "Segment: ", trans.proj$Segment, "<br>"))  %>%

  addProviderTiles("Esri.WorldImagery") %>%

  addCircleMarkers(data=data,
                   radius = 3,
                   color = ~pal2(Observer),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup= paste(data$Observer, "<br>", data$Species,
                                "<br>", data$Obs_Type, "<br>", data$Num)
  ) %>%


  addScaleBar()

print(map)
}



#' Calculate index estimates for a given year of WBPHS data
#'
#' EstimatesTableWBPHS will load a given year of greenlighted data and calculate an index estimate using a ratio estimator
#'
#' EstimatesTableWBPHS provides a shortcut to running piecewise functions for creating an annual index estimate for the WBPHS survey.
#' The function uses MasterFileList_WBPHS as a lookup table to locate the appropriate raw observation files, then calls ReadWBPHS to concatenate,
#' SummaryWBPHS for spatial design summary, TransDataWBPHS to table observations appropriately, and finally EstimatesWBPHS to calculate the ratio
#' estimate.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param year Four digit year of the WBPHS survey to use
#'
#' @return List object with 2 elements: \enumerate{
#' \item output.table Observer-specific estimates for the species indicated in the sppntable estimates column
#' \item expanded.table Deeper level count information by transect, strata, species, and observer
#' }
#'
#' @export
EstimatesTableWBPHS=function(year){

  entries=MasterFileList_WBPHS[MasterFileList_WBPHS$YEAR %in% year,]

  by.year=aggregate(entries$DRIVE~entries$YEAR,FUN=length)
  colnames(by.year)=c("YEAR", "COUNT")


  for (i in 1:length(by.year$YEAR)){

    temp.entries=entries[entries$YEAR==by.year$YEAR[i],]
    print(temp.entries)

    for (j in 1:by.year$COUNT[i]){

      if(j==1){

        data.path=paste(temp.entries$DRIVE[j], temp.entries$OBS[j], sep="")
        if(!file.exists(data.path)){next}
        data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)

      }

      if(j != 1){

        data.path=paste(temp.entries$DRIVE[j], temp.entries$OBS[j], sep="")
        if(!file.exists(data.path)){next}
        temp.data=read.csv(data.path, header=TRUE, stringsAsFactors = FALSE)

        data=rbind(data, temp.data)
      }

      data$Observer=paste(unique(data$Observer), collapse="_")

    }


    formatted=ReadWBPHS(data)

    flight=SummaryWBPHS(formatted)


    transdata=TransDataWBPHS(formatted)

    estimate=EstimatesWBPHS(transdata, flight=flight)


    if(i==1){output.table=estimate$estimates
    expanded.table=estimate$expanded}
    if(i>1){output.table=rbind(output.table, estimate$estimates)
    expanded.table=rbind(expanded.table, estimate$expanded)}

  }

  output.table$area="WBPHS"
  expanded.table$area="WBPHS"



  return(list(output.table=output.table, expanded.table=expanded.table))


}




WBPHStidy = function(adj, flight){

  trans.f = flight %>%
    group_by(Year, Stratum, Transect) %>%
    summarise(SampledArea = sum(SampledArea))

  trans.adj = adj %>%
    group_by(Year, Stratum, Transect, Species) %>%
    filter(!(Species %in% c("START", "END", "NoBirdsSeen"))) %>%
    summarise(total = sum(total), itotal = sum(itotal), ibb=sum(ibb), sing1pair2=sum(sing1pair2), flock=sum(flock))

  trans.merge = merge(trans.adj, trans.f) %>%
    complete(Year,  nesting(Stratum, Transect), Species, fill=list(total=0, itotal=0, ibb=0, sing1pair2=0, flock=0)) %>%
    group_by(Stratum, Transect) %>%
    fill(SampledArea, .direction=c("updown"))

  eider = trans.merge %>%
    filter(Species %in% c("COEI", "STEI", "KIEI", "SPEI", "UNEI")) %>%
    group_by(Year, Stratum, Transect, SampledArea) %>%
    summarise(Species="Eider", total=sum(total), itotal=sum(itotal), ibb=sum(ibb), sing1pair2=sum(sing1pair2), flock=sum(flock))

  merganser = trans.merge %>%
    filter(Species %in% c("COME", "RBME", "UNME")) %>%
    group_by(Year, Stratum, Transect, SampledArea) %>%
    summarise(Species="Merganser", total=sum(total), itotal=sum(itotal), ibb=sum(ibb), sing1pair2=sum(sing1pair2), flock=sum(flock))

  scoter = trans.merge %>%
    filter(Species %in% c("SCOT", "WWSC", "SUSC", "BLSC")) %>%
    group_by(Year, Stratum, Transect, SampledArea) %>%
    summarise(Species="Scoter", total=sum(total), itotal=sum(itotal), ibb=sum(ibb), sing1pair2=sum(sing1pair2), flock=sum(flock))

  grebe = trans.merge %>%
    filter(Species %in% c("HOGR", "RNGR", "UNGR")) %>%
    group_by(Year, Stratum, Transect, SampledArea) %>%
    summarise(Species="Grebe", total=sum(total), itotal=sum(itotal), ibb=sum(ibb), sing1pair2=sum(sing1pair2), flock=sum(flock))


  trans.merge = rbind(trans.merge, eider, merganser, scoter, grebe)

  by.stratum = trans.merge %>%
    group_by(Year, Stratum, Species) %>%
    summarise(total.density = sum(total)/sum(SampledArea),
              itotal.density = sum(itotal)/sum(SampledArea),
              ibb.density = sum(ibb)/sum(SampledArea),
              sing1pair2.density = sum(sing1pair2)/sum(SampledArea),
              flock.density = sum(flock)/sum(SampledArea),
              n = length(Transect),
              total.numerator = sum(total^2)-2*sum(total)*sum(total*SampledArea)/sum(SampledArea)+((sum(total)/sum(SampledArea))^2)*sum(SampledArea^2),
              itotal.numerator = sum(itotal^2)-2*sum(itotal)*sum(itotal*SampledArea)/sum(SampledArea)+((sum(itotal)/sum(SampledArea))^2)*sum(SampledArea^2),
              ibb.numerator = sum(ibb^2)-2*sum(ibb)*sum(ibb*SampledArea)/sum(SampledArea)+((sum(ibb)/sum(SampledArea))^2)*sum(SampledArea^2),
              sing1pair2.numerator = sum(sing1pair2^2)-2*sum(sing1pair2)*sum(sing1pair2*SampledArea)/sum(SampledArea)+((sum(sing1pair2)/sum(SampledArea))^2)*sum(SampledArea^2),
              flock.numerator = sum(flock^2)-2*sum(flock)*sum(flock*SampledArea)/sum(SampledArea)+((sum(flock)/sum(SampledArea))^2)*sum(SampledArea^2),
              denominator= n*(n-1)*(mean(SampledArea)^2))

  strata.area=data.frame("Stratum"=c(1, 2,3,4,5,6,7,8,9,99,10,11,12), "Area"=c(2200,3900,9300,10800,3400,4100,400,9900,26600,21637.937,3850,5350,1970))

  by.stratum = merge(by.stratum, strata.area)

  estimates = by.stratum %>%
    mutate(total.est = total.density * Area,
           total.var = Area^2 * total.numerator / denominator,
           total.se = sqrt(total.var),
           itotal.est = itotal.density * Area,
           itotal.var = Area^2 * itotal.numerator / denominator,
           ibb.est = ibb.density * Area,
           ibb.var = Area^2 * ibb.numerator / denominator,
           sing1pair2.est = sing1pair2.density * Area,
           sing1pair2.var = Area^2 * sing1pair2.numerator / denominator,
           flock.est = flock.density * Area,
           flock.var = Area^2 * flock.numerator / denominator)


  vcf = WBPHS_VCF %>%
    select(SPECIES, STRATUM, VCF, VCF_SE) %>%
    mutate(VCF.var = VCF_SE^2) %>%
    rename(Species=SPECIES, Stratum = STRATUM)

  estimates = merge(estimates, vcf, all.x=TRUE)


  estimates = estimates %>%
    mutate(adj.total = total.est * VCF,
           adj.total.se = sqrt((VCF^2*total.var+total.density*VCF.var-total.var*VCF.var)),
           adj.itotal = itotal.est * VCF ,
           adj.itotal.se = sqrt((VCF^2*itotal.var+itotal.density*VCF.var-itotal.var*VCF.var)) )

  return(estimates)

}




WBPHSbyYear = function(year){

  entries = MasterFileList_WBPHS %>%
    filter(YEAR==year)

  for (i in 1:length(entries$YEAR)){

    if(i == 1){full.data=read.csv(paste(entries$DRIVE[i], entries$OBS[i], sep=""), header=TRUE, stringsAsFactors = FALSE)}

    if(i != 1){temp.data=read.csv(paste(entries$DRIVE[i], entries$OBS[i], sep=""), header=TRUE, stringsAsFactors = FALSE)
    full.data=rbind(full.data, temp.data)
    }

  }


  processed = ReadWBPHS(full.data)

  flight = SummaryWBPHS(processed)

  est = WBPHStidy(processed, flight)

  return(est)
}


WBPHSMultipleYear= function(years){

  for (i in years){
    if (!(i %in% MasterFileList_WBPHS$YEAR)){next}
    if (i %in% MasterFileList_WBPHS$YEAR){
      if(i == years[1]){
        print(i)
        EstimatesTableWBPHS=WBPHSbyYear(i)
      }
      if(i != years[1]){
        print(i)
        temp.table=WBPHSbyYear(i)
        EstimatesTableWBPHS=rbind(EstimatesTableWBPHS, temp.table)
      }

    }
  }

  write.csv(EstimatesTableWBPHS, "EstimatesTableWBPHS.csv", row.names = FALSE, quote=FALSE)

}
