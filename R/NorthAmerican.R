

ReadWBPHS= function(folder.path){

  setwd(folder.path)

  full.data=data.frame(
    "yr"=numeric(),
    "mo"=numeric(),
    "day"=numeric(),
    "se"=character(),
    "obs"=character(),
    "strata"=character(),
    "tran"=character(),
    "seg"=character(),
    "dir"=numeric(),
    "a_g_name"=character(),
    "wind"=character(),
    "wspd"=numeric(),
    "sky"=character(),
    "wavfile"=character(),
    "lat"=numeric(),
    "long"=numeric(),
    "ctime"=numeric(),
    "lag"=numeric(),
    "sppn"=character(),
    "grp"=numeric(),
    "unit"=character(), stringsAsFactors = FALSE)




  file_list <- list.files(folder.path)

  for (file in file_list){


    type <- file_ext(file)

    if(type == "docx") {
      print(paste("Skipped", file))
      next}


    print(paste("Included file", file, "successfully."))

      temp.data <-read.csv(file, header=FALSE)
      colnames(temp.data)=colnames(full.data)
      full.data<-rbind(full.data, temp.data)


  }

  full.data=Cut9(full.data)

  full.data=AdjustCounts(full.data)

  full.data$sppn=as.character(full.data$sppn)

  full.data$sppn[full.data$sppn %in% c("SUSC1", "SUSC2", "WWSC1", "WWSC2", "SCOT1", "SCOT2", "SUSC",
                           "WWSC", "BLSC", "BLSC1", "BLSC2")]="SCOT"

  full.data$sppn[full.data$sppn %in% c("HOGR", "RNGR")]="GREB"



  return(full.data)

}


Cut9= function(full.data, copy=TRUE){


long.cut <- data.frame(cbind(rep(9,8),
                            seq(12,19,1),
                            c(-163.559,-163.738,-164.388,-164.130,-164.440,-164.995,-164.938,-164.810)))

names(long.cut) <- c("strata","tran","long")

temp.data=full.data[full.data$sppn %in% c("CAGO", "GWFG", "SWAN") & full.data$strata=="9" & full.data$tran %in% long.cut$tran,]

temp.data$strata="9c"
temp.data$keep="y"

for (i in 1:length(temp.data$long)){
  if(temp.data$long[i]<long.cut$long[temp.data$tran[i]==long.cut$tran]){temp.data$keep[i]="n"}

}

temp.data=temp.data[temp.data$keep=="y",]
temp.data=subset(temp.data, select=-keep)

full.data=rbind(full.data, temp.data)

return(full.data)
}


SummaryWBPHS=function(full.data){

  flight=data.frame(

    yr=numeric(),
    obs=character(),
    strata=character(),
    tran=character(),
    n.segs=numeric(),
    len=numeric(),
    sampled.area=numeric(), stringsAsFactors = FALSE
  )



  for (observer in unique(full.data$obs)){

    temp.data=full.data[selected.data$obs==observer,]
    st.sets=(unique(temp.data[,c("strata", "tran")]))
    st.sets$n.segs=0
    st.sets$len=0


    for(i in 1:length(st.sets[,1])){

      st.sets$n.segs[i]=length(unique(temp.data$seg[temp.data$strata==st.sets$strata[i] & temp.data$tran==st.sets$tran[i]]))

      seg.len=16

      if(st.sets$strata[i]==7){seg.len=8}
      if(st.sets$strata[i]==12){seg.len=18}

      st.sets$len[i]=seg.len*st.sets$n.segs[i]

    }


    temp.flight=data.frame(

      yr=rep(temp.data$yr[1], length(st.sets[,1])),
      obs=rep(temp.data$obs[1], length(st.sets[,1])),
      strata=st.sets$strata,
      tran=st.sets$tran,
      n.segs=st.sets$n.segs,
      len=st.sets$len, stringsAsFactors = FALSE
    )


    temp.flight$sampled.area=temp.flight$len*.125

    flight=rbind(flight, temp.flight)

  }


  for (i in 1:length(flight$strata)){

    if(flight$strata[i]=="9c"){

      if(flight$tran[i]=="12"){flight$sampled.area[i]=22}
      if(flight$tran[i]=="13"){flight$sampled.area[i]=20.763}
      if(flight$tran[i]=="14"){flight$sampled.area[i]=21.32}
      if(flight$tran[i]=="15"){flight$sampled.area[i]=12}
      if(flight$tran[i]=="16"){flight$sampled.area[i]=14.543}
      if(flight$tran[i]=="17"){flight$sampled.area[i]=8.395}
      if(flight$tran[i]=="18"){flight$sampled.area[i]=7.663}
      if(flight$tran[i]=="19"){flight$sampled.area[i]=8.372}



    }

  }



  return(flight)
}



TransDataWBPHS=function(selected.data){

    #groupings list
    unit.list=c("single", "pair","open", "flkdrake")

    #list of species
    sp.list=as.character(unique(selected.data$sppn))

    #grid method


    for (observer in unique(selected.data$obs)){

      temp.data=selected.data[selected.data$obs==observer,]
      sets=(unique(temp.data[,c("strata", "tran", "seg")]))


      for (n in 1:length(sets[,1])){
        new.rows=expand.grid(temp.data$yr[1], NA, NA, NA, observer, sets[n,1], sets[n,2], sets[n,3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, sp.list, 0, unit.list, 0,0,0,0)


        names(new.rows)=names(temp.data)
        selected.data=rbind(selected.data,new.rows)


      }


    }


    selected.data$grp=as.numeric(selected.data$grp)

    agg=aggregate(cbind(grp,itotal,total,ibb, sing1pair2)~yr+obs+sppn+unit+strata+tran+seg,data=selected.data, FUN=sum)

    colnames(agg)=c("yr", "obs", "sppn", "unit", "strata", "tran", "seg", "grp", "itotal", "total", "ibb", "sing1pair2")

    return(agg[order(agg$yr, agg$obs, agg$sppn, agg$strata, as.numeric(agg$tran), as.numeric(agg$seg), agg$unit),])

  }







CountsWBPHS=function(adj.counts) {



  t1=(aggregate(adj.counts$total~adj.counts$yr+adj.counts$obs+adj.counts$tran+adj.counts$sppn+adj.counts$strata, FUN=sum))
  t2=(aggregate(adj.counts$itotal~adj.counts$yr+adj.counts$obs+adj.counts$tran+adj.counts$sppn+adj.counts$strata, FUN=sum))
  t2b=aggregate(adj.counts$ibb~adj.counts$yr+adj.counts$obs+adj.counts$tran+adj.counts$sppn+adj.counts$strata, FUN=sum)
  t4=aggregate(adj.counts$sing1pair2~adj.counts$yr+adj.counts$obs+adj.counts$tran+adj.counts$sppn+adj.counts$strata, FUN=sum)
  t3=merge(t1,t2,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$tran", "adj.counts$sppn", "adj.counts$strata"))
  t3=merge(t3,t2b,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$tran", "adj.counts$sppn", "adj.counts$strata"))
  t3=merge(t3,t4,by=c("adj.counts$yr", "adj.counts$obs","adj.counts$tran", "adj.counts$sppn", "adj.counts$strata"))


  colnames(t3)=c("yr","obs","tran", "sppn", "strata", "total", "itotal", "ibb", "sing1pair2")

  return(t3[order(t3$yr, t3$obs, t3$sppn, t3$strata, as.numeric(t3$tran)),])




}



EstimatesWBPHS=function(data, flight){



  counts.t=CountsWBPHS(data)
  counts.t$area=0

  for (i in 1:length(counts.t$area)){

    counts.t$area[i]=flight$sampled.area[flight$yr==counts.t$yr[i] & flight$obs==counts.t$obs[i] & flight$tran==counts.t$tran[i] & flight$strata==counts.t$strata[i]][1]

  }

  t3=counts.t
  #Remove transect start and end from species list
  t3=t3[t3$sppn != "NoBirdsSeen",]
  t3=t3[t3$sppn != "BEGCOUNT",]
  t3=t3[t3$sppn != "ENDCOUNT",]


  #t3=t3[t3$sppn != "end",]
  #t3=t3[t3$sppn != "start",]
  #t3=t3[t3$sppn != "ENDT",]
  #t3=t3[t3$sppn != "BEGT",]

  #Make sure totals are numeric
  t3$total=as.numeric(t3$total)
  t3$itotal=as.numeric(t3$itotal)
  t3$ibb=as.numeric(t3$ibb)
  t3$sing1pair2=as.numeric(t3$sing1pair2)

  sp.strat.total=aggregate(t3$total~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.total)=c("yr","obs", "sppn", "strata", "total")

  sp.strat.itotal=aggregate(t3$itotal~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.itotal)=c("yr","obs", "sppn", "strata", "itotal")

  sp.strat.ibb=aggregate(t3$ibb~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.ibb)=c("yr","obs", "sppn", "strata", "ibb")

  sp.strat.sing1pair2=aggregate(t3$sing1pair2~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=sum)
  colnames(sp.strat.sing1pair2)=c("yr","obs", "sppn", "strata", "sing1pair2")

  #Variance of the counts within each strata
  sp.strat.total.v=aggregate(t3$total~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.total.v)=c("yr", "obs", "sppn", "strata", "total.v")

  sp.strat.itotal.v=aggregate(t3$itotal~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.itotal.v)=c("yr", "obs", "sppn", "strata", "itotal.v")

  sp.strat.ibb.v=aggregate(t3$ibb~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.ibb.v)=c("yr","obs", "sppn", "strata", "ibb.v")

  sp.strat.sing1pair2.v=aggregate(t3$sing1pair2~t3$yr+t3$obs+t3$sppn+t3$strata, FUN=var)
  colnames(sp.strat.sing1pair2.v)=c("yr","obs", "sppn", "strata", "sing1pair2.v")

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

  for (i in 1:length(sp.strat.final$strata)){

    sp.strat.final$ssq.total[i]=sum(t3$total[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]]^2)
    sp.strat.final$ssq.itotal[i]=sum(t3$itotal[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]]^2)
    sp.strat.final$ssq.ibb[i]=sum(t3$ibb[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]]^2)
    sp.strat.final$ssq.sing1pair2[i]=sum(t3$sing1pair2[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]]^2)

    sp.strat.final$cxa.total[i]=sum(t3$total[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]] * t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]])
    sp.strat.final$cxa.itotal[i]=sum(t3$itotal[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]] * t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]])
    sp.strat.final$cxa.ibb[i]=sum(t3$ibb[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]] * t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]] )
    sp.strat.final$cxa.sing1pair2[i]=sum(t3$sing1pair2[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]] * t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]])

    sp.strat.final$ssq.area[i]=sum(t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]]^2)

    sp.strat.final$mean.area[i]=mean(t3$area[t3$sppn==sp.strat.final$sppn[i] & t3$strata==sp.strat.final$strata[i]])

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

  area.strat=aggregate(t3$area~t3$yr+t3$obs+t3$strata+t3$sppn, FUN=sum)
  area.strat.v=aggregate(t3$area~t3$yr+t3$obs+t3$strata+t3$sppn, FUN=var)

  colnames(area.strat)=c("yr", "obs", "strata", "sppn","total.area")
  colnames(area.strat.v)=c("yr", "obs", "strata", "sppn", "total.area.var")

  area.strat=area.strat[!duplicated(area.strat[1:3]),-4]
  area.strat.v=area.strat.v[!duplicated(area.strat.v[1:3]),-4]


  #Put spatial summary together
  area.summary=merge(area.strat, area.strat.v)
  #print(area.summary)


  #Merge the counts and spatial stats
  counts.final=merge(sp.strat.final,area.summary, by=c("yr", "obs", "strata"))

  #Calculate final densities for each strata layer
  density.total=counts.final$total/counts.final$total.area
  density.itotal=counts.final$itotal/counts.final$total.area
  density.ibb=counts.final$ibb/counts.final$total.area
  density.sing1pair2=counts.final$sing1pair2/counts.final$total.area


  counts.final=cbind(counts.final, density.total, density.itotal, density.ibb, density.sing1pair2)
  #print(head(counts.final))


  strata.area=data.frame("strata"=c("1", "2","3","4","5","6","7","8","9","9c","10","11","12"), "layer.area"=c(2200,3900,9300,10800,3400,4100,400,9900,26600,21637.937,3850,5350,1970))
  counts.final=merge(counts.final, strata.area, by="strata")


  #Extrapolate density estimates across area calculation
  total.est=counts.final$density.total * counts.final$layer.area
  itotal.est=counts.final$density.itotal * counts.final$layer.area
  ibbtotal.est=counts.final$density.ibb * counts.final$layer.area
  sing1pair2.est=counts.final$density.sing1pair2 * counts.final$layer.area

  counts.final=cbind(counts.final, total.est, itotal.est,ibbtotal.est, sing1pair2.est)


  #Summarize in table
  estimates=aggregate(counts.final$total.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates)=c("yr", "obs", "sppn", "total.est")

  estimates.i=aggregate(counts.final$itotal.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.i)=c("yr", "obs", "sppn","itotal.est")

  estimates.ibb=aggregate(counts.final$ibbtotal.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.ibb)=c("yr", "obs", "sppn","ibbtotal.est")

  estimates.sing1pair2=aggregate(counts.final$sing1pair2.est~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
  colnames(estimates.sing1pair2)=c("yr", "obs", "sppn","sing1pair2.est")

  estimates=merge(estimates, estimates.i, by=c("yr", "obs", "sppn"))
  estimates=merge(estimates, estimates.ibb, by=c("yr", "obs", "sppn"))
  estimates=merge(estimates, estimates.sing1pair2, by=c("yr", "obs", "sppn"))

  #Strata level estimates organized





  diff.lat=area.summary

  diff.lat$m=diff.lat$total.area
  diff.lat$M=0

  for (i in 1:length(diff.lat$strata)){

    diff.lat$M[i]=strata.area$layer.area[strata.area$strata==diff.lat$strata[i]]/diff.lat$m[i]


  }

  #See equation 12.9, p. 249 in "Analysis and Management of Animal Populations"
  #Williams, Nichols, Conroy; 2002

  for (j in 1:length(counts.final$sppn)){
#
#     M=diff.lat$M[diff.lat$yr==counts.final$yr[j] & diff.lat$obs==counts.final$obs[j] & diff.lat$strata==counts.final$strata[j]]
#     m=diff.lat$m[diff.lat$yr==counts.final$yr[j] & diff.lat$obs==counts.final$obs[j] & diff.lat$strata==counts.final$strata[j]]
#     prop.m=((1-(m/M))/m)

    #if(counts.final$sppn[j]=="SPEI"){print((counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j])))}
#
#     counts.final$var.N[j]=(M^2)*prop.m*(counts.final$total.v[j]+(counts.final$density.total[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.total[j]*counts.final$total.cov[j]))
#     counts.final$var.Ni[j]=(M^2)*prop.m*(counts.final$itotal.v[j]+(counts.final$density.itotal[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.itotal[j]*counts.final$itotal.cov[j]))
#     counts.final$var.Nib[j]=(M^2)*prop.m*(counts.final$ibb.v[j]+(counts.final$density.ibb[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.ibb[j]*counts.final$ibb.cov[j]))
#     counts.final$var.Nsing1pair2[j]=(M^2)*prop.m*(counts.final$sing1pair2.v[j]+(counts.final$density.sing1pair2[j]^2)*(counts.final$total.area.var[j])-(2*counts.final$density.sing1pair2[j]*counts.final$sing1pair2.cov[j]))
#

    counts.final$var.N[j]=counts.final$layer.area[j]^2*(counts.final$ssq.total[j]-2*counts.final$density.total[j]*counts.final$cxa.total[j]+(counts.final$density.total[j]^2)*(counts.final$ssq.area[j]))/(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])*(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Ni[j]=counts.final$layer.area[j]^2*(counts.final$ssq.itotal[j]-2*counts.final$density.itotal[j]*counts.final$cxa.itotal[j]+(counts.final$density.itotal[j]^2)*(counts.final$ssq.area[j]))/(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])*(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Nib[j]=counts.final$layer.area[j]^2*(counts.final$ssq.ibb[j]-2*counts.final$density.ibb[j]*counts.final$cxa.ibb[j]+(counts.final$density.ibb[j]^2)*(counts.final$ssq.area[j]))/(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])*(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])-1)*counts.final$mean.area[j]^2)
    counts.final$var.Nsing1pair2[j]=counts.final$layer.area[j]^2*(counts.final$ssq.sing1pair2[j]-2*counts.final$density.sing1pair2[j]*counts.final$cxa.sing1pair2[j]+(counts.final$density.sing1pair2[j]^2)*(counts.final$ssq.area[j]))/(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])*(length(t3$tran[t3$strata==counts.final$strata[j] & t3$sppn==counts.final$sppn[j]])-1)*counts.final$mean.area[j]^2)




  }






#
#   var.est=aggregate(counts.final$var.N~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
#   colnames(var.est)=c("yr", "obs", "sppn","var.N")
#
#   var.est.i=aggregate(counts.final$var.Ni~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
#   colnames(var.est.i)=c("yr", "obs", "sppn","var.Ni")
#
#   var.est.ibb=aggregate(counts.final$var.Nib~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
#   colnames(var.est.ibb)=c("yr", "obs", "sppn","var.Nib")
#
#   var.est.sing1pair2=aggregate(counts.final$var.Nsing1pair2~counts.final$yr+counts.final$obs+counts.final$sppn, FUN=sum)
#   colnames(var.est.sing1pair2)=c("yr", "obs", "sppn","var.Nsing1pair2")

  estimates=merge(estimates, var.est, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.i, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.ibb, by=c("yr", "obs", "sppn"), all=TRUE)
  estimates=merge(estimates, var.est.sing1pair2, by=c("yr", "obs", "sppn"), all=TRUE)


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


  return(counts.final[order(counts.final$sppn, counts.final$obs, counts.final$strata),])


  #return(list("estimates"=estimates, "diff.lat"=diff.lat, "strata.area"=strata.area, "counts.final"=counts.final))

}


