

ReadWBPHS= function(full.data){

  full.data=full.data[full.data$Code==1,]

  full.data=Cut9(full.data)

  full.data=full.data[full.data$keep==1,]

  full.data$Obs_Type[full.data$Obs_Type=="open" & full.data$Num==1 & full.data$Species!="SWANN"]="single"

  full.data$Obs_Type[full.data$Obs_Type=="open" & full.data$Num==2]="pair"

  full.data=AdjustCounts(full.data)

  return(full.data)

}


Cut9= function(full.data, copy=TRUE){

full.data$keep=1

long.cut <- data.frame(cbind(rep(9,8),
                            seq(12,19,1),
                            c(-163.559,-163.738,-164.388,-164.130,-164.440,-164.995,-164.938,-164.810)))

names(long.cut) <- c("strata","tran","long")


for(i in 1:length(full.data$Species)){

  if(full.data$Species[i] %in% c("CCGO", "GWFG", "SWAN") &
     full.data$Stratum[i]==9 &
    full.data$Transect[i] %in% long.cut$tran){

    full.data$Stratum[i]=99
    if(full.data$Lon[i]<long.cut$long[full.data$Transect[i]==long.cut$tran]){full.data$keep[i]=0}



  }




}


# temp.data=full.data[full.data$sppn %in% c("CCGO", "GWFG", "SWAN") & full.data$strata=="9" & full.data$tran %in% long.cut$tran,]
#
# temp.data$strata="9c"
# temp.data$keep="y"
#
# for (i in 1:length(temp.data$long)){
#   if(temp.data$long[i]<long.cut$long[temp.data$tran[i]==long.cut$tran]){temp.data$keep[i]="n"}
#
# }
#
# temp.data=temp.data[temp.data$keep=="y",]
# temp.data=subset(temp.data, select=-keep)
#
# full.data=rbind(full.data, temp.data)

return(full.data)
}


SummaryWBPHS=function(full.data, strip.width=0.125){

  flight=data.frame(

    Year=numeric(),
    Observer=character(),
    Strata=character(),
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
      Strata=st.sets$Stratum,
      Transect=st.sets$Transect,
      n.segs=st.sets$n.segs,
      Length=st.sets$Length, stringsAsFactors = FALSE
    )


    temp.flight$SampledArea=temp.flight$Length*strip.width

    flight=rbind(flight, temp.flight)

  }


  for (i in 1:length(flight$Strata)){

    if(flight$Strata[i]==99){

      if(flight$Transect[i]==12){flight$SampledArea[i]=22 * (strip.width/.125)}
      if(flight$Transect[i]==13){flight$SampledArea[i]=20.763* (strip.width/.125)}
      if(flight$Transect[i]==14){flight$SampledArea[i]=21.32* (strip.width/.125)}
      if(flight$Transect[i]==15){flight$SampledArea[i]=12* (strip.width/.125)}
      if(flight$Transect[i]==16){flight$SampledArea[i]=14.543* (strip.width/.125)}
      if(flight$Transect[i]==17){flight$SampledArea[i]=8.395* (strip.width/.125)}
      if(flight$Transect[i]==18){flight$SampledArea[i]=7.663* (strip.width/.125)}
      if(flight$Transect[i]==19){flight$SampledArea[i]=8.372* (strip.width/.125)}



    }

  }



  return(flight)
}



TransDataWBPHS=function(selected.data){

    agg=aggregate(cbind(Num,itotal,total,ibb, sing1pair2)~Year+Observer+Species+Obs_Type+Stratum+Transect+Segment,data=selected.data, FUN=sum)


    colnames(agg)=c("Year", "Observer", "Species", "Obs_Type", "Stratum", "Transect", "Segment", "Num", "itotal", "total", "ibb", "sing1pair2")

    agg=as.data.frame(complete(data=agg, Year, Observer, Species, Obs_Type, nesting(Stratum, Transect, Segment), fill=list(Num=0, itotal=0, total=0, ibb=0, sing1pair2=0)))

    agg$area=0

    return(agg[order(agg$Year, agg$Observer, agg$Species, agg$Stratum, agg$Transect, agg$Segment, agg$Obs_Type),])

  }

  #   #groupings list
  #   unit.list=c("single", "pair","open", "flkdrake")
  #
  #   #list of species
  #   sp.list=as.character(unique(selected.data$Species))
  #
  #   #grid method
  #
  #
  #   for (observer in unique(selected.data$obs)){
  #
  #     temp.data=selected.data[selected.data$obs==observer,]
  #     sets=(unique(temp.data[,c("strata", "tran", "seg")]))
  #
  #
  #     for (n in 1:length(sets[,1])){
  #       new.rows=expand.grid(temp.data$yr[1], NA, NA, NA, observer, sets[n,1], sets[n,2], sets[n,3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list, 0, unit.list, 0,0,0,0)
  #
  #
  #       names(new.rows)=names(temp.data)
  #       selected.data=rbind(selected.data,new.rows)
  #
  #
  #     }
  #
  #
  #   }
  #
  #
  #   selected.data$grp=as.numeric(selected.data$grp)
  #
  #   agg=aggregate(cbind(grp,itotal,total,ibb, sing1pair2)~yr+obs+sppn+unit+strata+tran+seg,data=selected.data, FUN=sum)
  #
  #   colnames(agg)=c("yr", "obs", "sppn", "unit", "strata", "tran", "seg", "grp", "itotal", "total", "ibb", "sing1pair2")
  #
  #   return(agg[order(agg$yr, agg$obs, agg$sppn, agg$strata, as.numeric(agg$tran), as.numeric(agg$seg), agg$unit),])
  #
  # }







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


  # for (i in 1:length(sp.strat.final$strata)){
  #
  #   temp.t3=t3[t3$yr==sp.strat.final$yr[i] & t3$obs==sp.strat.final$obs[i] & t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i],]
  #   sp.strat.final$total.cov[i]=cov(temp.t3$total, temp.t3$area)
  #   sp.strat.final$itotal.cov[i]=cov(temp.t3$itotal, temp.t3$area)
  #   sp.strat.final$ibb.cov[i]=cov(temp.t3$ibb, temp.t3$area)
  #   sp.strat.final$sing1pair2.cov[i]=cov(temp.t3$sing1pair2, temp.t3$area)
  #
  # }


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



ShowWBPHS=function(estimates.by.strata, data, trans.file="Q:/Waterfowl/WBPHS/Design Files/Design_Transects/NAWBPS_segments.shp"){

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
