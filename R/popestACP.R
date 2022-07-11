#' A function to calculate variance of ACP population estimates 
#' @param dat A data frame containing transect-level sums of observed singles and pairs, see details. 
#' @param pData A data frame containing visibility correction factors by strata, see details. 
#' @param Mdat A data frame ... giving the total number of possible transects in each strata. Names must match strata names in data. 
#' @param prob A logical indicating where to do the calculation based on detection probability or it reciprocal, vcf. 
#' @return A list containing population estimate of 'indicated breeding birds', the 'index" estimates (without detection correction), standard errors of each, and a ggplot objects of each by year.
#' @details 
#'
#' @examples
popestACP <- function(dat=NA, pData=NA, Mdat=NA){
  #Code to calculate detection-corrected population estimates from stratified plot data 
  # using a ratio estimator. The approach is from Fieberg and Giudice (2008, JWM 72:837), 
  # hereafter F&G. 
  #The key parts are equations A3 and A4 of F&G.  A3 is explained in F&G; 
  # A4 is explained best in the text book of Thompson (2012, Sampling).  
  #Key difference from above citation is the use of the ratio estimator 
  #for the mean number observed across transects.  This was not in F&G or Thompson.
  
  #Code originally written by Erik Osnas, December 2018. Modified in January & February 2019.
  #Modified July and August 2019 for data request from Kylee Durham.
  # write as function that accepts dataframe of transect level summaries, and out put VCF corrected population estimates.
  #Copyright:  GNU General Public License v3.0
  
  #this version is for use with ACP data and detection probability estimates by group size, 
  # not VCF and density strata
  # here on ACP observer is also a 'strata' that shares detection estimate. 
  library(ggplot2)
  library(dplyr)
  
  #trim to flkdrake and open >= 5, 
  #  there are only 3 observation of flkdrakes of size 3 or 4 in data
  # flkdakes will be doubled for indicated breeding birds
  #2 flkdrakes are treated as pairs in function popestACP so that they get the pair detection rate. 
  #singles, pairs, and flkdrake <=4 are doubled for indicated breeding birds 
  # opens are ignored
  trim <- dat$Obs_Type == "flkdrake" & dat$Num >= 5
  dat <- dat[!trim, ]
  trim <- dat$Obs_Type == "open"
  dat <- dat[!trim, ]
  
  sing <- dat[dat$Obs_Type=="single" , ] 
  names(sing)[11] <- "Singles"
  pair <- dat[dat$Obs_Type=="pair" , ]
  names(pair)[11] <- "Pairs"
  dat2 <- data.frame(sing[,-c(4,6)], Singles=sing[,"Singles"], Pairs=pair[,"Pairs"])
  
  #flkdrakes 2 are treated as pairs in function popestACP for detection so that they get the pair detection rate. 
  #singles, pairs, and flkdrake 2 are doubled for indicated breeding birds 
  fdop1 <- dat[ (dat$Obs_Type=="flkdrake" & dat$Num==1), ] 
  names(fdop1)[11] <- "fdop1"
  fdop1 <- fdop1[,-4]
  
  fdop2 <- dat[ (dat$Obs_Type=="flkdrake" & dat$Num==2), ] 
  names(fdop2)[11] <- "fdop2"
  fdop2 <- fdop2[,-4]
  
  fdopsf3 <- dat[ (dat$Obs_Type=="flkdrake" & dat$Num==3), ] 
  names(fdopsf3)[11] <- "fdopsf3"
  fdopsf3 <- fdopsf3[,-4]
  
  fdopsf4 <- dat[ (dat$Obs_Type=="flkdrake" & dat$Num==4), ] 
  names(fdopsf4)[11] <- "fdopsf4"
  fdopsf4 <- fdopsf4[,-4]
  
  dat2 <- left_join(dat2, fdop1[,c(1:4,10)])
  dat2$fdop1 <- ifelse(is.na(dat2$fdop1),0,dat2$fdop1)
  dat2 <- left_join(dat2, fdop2[,c(1:4,10)])
  dat2$fdop2 <- ifelse(is.na(dat2$fdop2),0,dat2$fdop2)
  dat2 <- left_join(dat2, fdopsf3[,c(1:4,10)])
  dat2$fdopsf3 <- ifelse(is.na(dat2$fdopsf3),0,dat2$fdopsf3)
  dat2 <- left_join(dat2, fdopsf4[,c(1:4,10)])
  dat2$fdopsf4 <- ifelse(is.na(dat2$fdopsf4),0,dat2$fdopsf4)
  
  dat2$Singles <- dat2$Singles + dat2$fdop1
  dat2$Pairs <- dat2$Pairs 
  dat2$SmFl2 <- dat2$fdop2 #2 flkdrake are treated as pairs for detection, both singles and pairs are doubled for ibb so this is OK
  dat2$SmFl3 <- dat2$fdopsf3
  dat2$SmFl4 <- dat2$fdopsf4
  dat <- dat2
  rm(dat2)
  
  ########################
  #find mean by year, statum, and observer
  msing <- aggregate(dat$Singles, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  mpair <- aggregate(dat$Pairs, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  msmfl2 <- aggregate(dat$SmFl2, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  msmfl3 <- aggregate(dat$SmFl3, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  msmfl4 <- aggregate(dat$SmFl4, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  mx <- aggregate(dat$obs.area, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean) #, na.rm=TRUE 
  Area <- aggregate(dat$strata.area, by=list(Year=dat$Year, strata=dat$strata, obs=dat$Observer), mean)
  Year=unique(dat$Year)
  
  #mvcf <- aggregate(dat$vcf, by=list(strata=dat$strata, Year=dat$Year), mean)
  
  #calculate "index" estimate, sum singles and pairs over strata by year
  #Area*msing/mx is the ratio estimate of the population total of observed singles and pairs
  # Yibb is twice the number of singles and pairs
  Ysing <- aggregate(Area$x*msing$x/mx$x, list(Year=mx$Year, obs=mx$obs), sum)
  Ysing <- aggregate(Ysing$x, list(Year=Ysing$Year), mean)
  Ypair <- aggregate(Area$x*mpair$x/mx$x, list(Year=mx$Year, obs=mx$obs), sum)
  Ypair <- aggregate(Ypair$x, list(Year=Ypair$Year), mean)
  Ysmfl2 <- aggregate(Area$x*msmfl2$x/mx$x, list(Year=mx$Year, obs=mx$obs), sum)
  Ysmfl2 <- aggregate(Ysmfl2$x, list(Year=Ysmfl2$Year), mean)
  Ysmfl3 <- aggregate(Area$x*msmfl3$x/mx$x, list(Year=mx$Year, obs=mx$obs), sum)
  Ysmfl3 <- aggregate(Ysmfl3$x, list(Year=Ysmfl3$Year), mean)
  Ysmfl4 <- aggregate(Area$x*msmfl4$x/mx$x, list(Year=mx$Year, obs=mx$obs), sum)
  Ysmfl4 <- aggregate(Ysmfl4$x, list(Year=Ysmfl4$Year), mean)
  Yibb <- 2*(Ysing$x + Ypair$x + 2*Ysmfl2$x + 3*Ysmfl3$x + 4*Ysmfl4$x)
  #plot(Year, Yibb, pch=16, type='b')
  
  #Find population estimate by applying Vdetection correction
  #For each strata in each year, multiply by VCF before summing over strata
  #need to make detection df by year and strata and group size
  
  p <- rep(pData$p[pData$strata=="Singles"], dim(msing)[1])
  Nsing <- aggregate(Area$x*msing$x/(mx$x*p), 
                     list(Year=mx$Year, obs=mx$obs), sum)
  p <- rep(pData$p[pData$strata=="Pairs"], dim(msing)[1])
  Npair <- aggregate(Area$x*mpair$x/(mx$x*p), 
                     list(Year=mx$Year, obs=mx$obs), sum)
  Nsmfl2 <- aggregate(Area$x*msmfl2$x/(mx$x*p), 
                      list(Year=mx$Year, obs=mx$obs), sum) #fd 2 treated as pairs
  p <- rep(pData$p[pData$strata=="SmFlocks"], dim(msing)[1])
  Nsmfl3 <- aggregate(Area$x*msmfl3$x/(mx$x*p), 
                     list(Year=mx$Year, obs=mx$obs), sum)
  Nsmfl4 <- aggregate(Area$x*msmfl4$x/(mx$x*p), 
                      list(Year=mx$Year, obs=mx$obs), sum)
  Nsing <- aggregate(Nsing$x, list(Year=Nsing$Year), mean)
  Npair <- aggregate(Npair$x, list(Year=Npair$Year), mean)
  Nsmfl2 <- aggregate(Nsmfl2$x, list(Year=Nsmfl2$Year), mean)
  Nsmfl3 <- aggregate(Nsmfl3$x, list(Year=Nsmfl3$Year), mean)
  Nsmfl4 <- aggregate(Nsmfl4$x, list(Year=Nsmfl4$Year), mean)
  Nibb <- 2*(Nsing$x + Npair$x + 2*Nsmfl2$x + 3*Nsmfl3$x + 4*Nsmfl4$x)
  #plot(Year, Nibb, pch=16, type='b', ylim=c(0, max(Nibb)))
  #lines(Year, Yibb, pch=22, type='b')
  ###############################################################################
  #Find variance of estimates
  #Need total number of possible transects for each strata. These vary by year. 

  #put this all together in a list so we can interate through by strata and year 
  #to find variance and covariance between counts and transect areas
  ss <- list(Year=msing$Year, strata=msing$strata, m = c(), M=c(), Area=Area$x, xVar=c(), sCov = c(), 
             sVar=c(), pVar=c(), pCov=c(), msing=msing$x, mpair=mpair$x, msmfl2=msmfl2$x, msmfl3=msmfl3$x, msmfl4=msmfl4$x, 
             mx=mx$x, obs=msing$obs )
  for(i in 1:length(msing$x)){
    tempdat <- dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i] & dat$Observer==msing$obs[i]), 
                   c("obs.area", "Singles")]
    ss$m[i] <- dim(tempdat)[1]
    ss$M[i] <- Mdat[Mdat$Year== msing$Year[i] & Mdat$strata==msing$strata[i], "M"][1]
    temp <- cov(as.matrix(tempdat),  use="na.or.complete")
    ss$xVar[i] <- temp[1,1]
    ss$sCov[i] <- temp[1,2]
    ss$sVar[i] <- temp[2,2]
    temp <- cov(as.matrix(dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                              c("obs.area", "Pairs")]), use="na.or.complete")
    ss$pCov[i] <- temp[1,2]
    ss$pVar[i] <- temp[2,2]
    temp <- cov(as.matrix(dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                              c("obs.area", "SmFl2")]), use="na.or.complete")
    ss$f2Cov[i] <- temp[1,2]
    ss$f2Var[i] <- temp[2,2]
    temp <- cov(as.matrix(dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                              c("obs.area", "SmFl3")]), use="na.or.complete")
    ss$f3Cov[i] <- temp[1,2]
    ss$f3Var[i] <- temp[2,2]
    
    temp <- cov(as.matrix(dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                              c("obs.area", "SmFl4")]), use="na.or.complete")
    ss$f4Cov[i] <- temp[1,2]
    ss$f4Var[i] <- temp[2,2]
    
    ss$psing[i] = pData[pData$strata=="Singles","p"]
    ss$psesing[i] = pData[pData$strata=="Singles","se"]
    ss$ppair[i] = pData[pData$strata=="Pairs","p"]
    ss$psepair[i] = pData[pData$strata=="Pairs","se"]
    ss$psmfl2[i] = pData[pData$strata=="Pairs","p"]
    ss$psesmfl2[i] = pData[pData$strata=="Pairs","se"]
    ss$psmfl[i] = pData[pData$strata=="SmFlocks","p"]
    ss$psesmfl[i] = pData[pData$strata=="SmFlocks","se"]
  }
  
  #########################
  #Calculate index variance
  #first sample variance of the mean number observed per transect.
  #The sample variance is eq. 7.5 from Thompson 2012.
  ss$VarIndexSing <- with(ss, { 
    (1 - m/M)*(sVar + (msing/mx)^2*xVar - 2*(msing/mx)*sCov)/m  }) 
  ss$VarIndexPair <- with(ss, { 
    (1 - m/M)*(pVar + (mpair/mx)^2*xVar - 2*(mpair/mx)*pCov)/m  })
  ss$VarIndexSmFl3 <- with(ss, { 
    (1 - m/M)*(f3Var + (msmfl3/mx)^2*xVar - 2*(msmfl3/mx)*f3Cov)/m  })
  ss$VarIndexSmFl2 <- with(ss, { 
    (1 - m/M)*(f2Var + (msmfl2/mx)^2*xVar - 2*(msmfl2/mx)*f2Cov)/m  })
  ss$VarIndexSmFl4 <- with(ss, { 
    (1 - m/M)*(f4Var + (msmfl4/mx)^2*xVar - 2*(msmfl4/mx)*f4Cov)/m  })
  #expand to all strata area; sum singles and pairs and flocks; convert to IBB
  ss$VarExpandIBB <- 4 * ss$M^2 * (ss$VarIndexSing + ss$VarIndexPair + 4 * ss$VarIndexPair + 
                                     9 * ss$VarIndexSmFl3 + 16 * ss$VarIndexSmFl4 )

  VarIndex <- aggregate(VarExpandIBB  ~ Year + obs, data=as.data.frame(ss), FUN=sum)
  #find number of observer each year
  numobs<-rowSums(table(VarIndex$Year, VarIndex$obs))
  VarIndex <- aggregate(VarExpandIBB  ~ Year, data=as.data.frame(ss), FUN=sum)[,2]/(numobs^2)
  # plot(Year, Nibb, pch=16, type='b', ylim=c(0, max(Nibb)))
  # lines(Year, Yibb, pch=22, type='b')
  # arrows(x0=Year, x1=Year, y0=Yibb-2*sqrt(VarIndex), y1=Yibb+2*sqrt(VarIndex), length=0)
  #End index variance 
  #########################
  #########################
  #Calculate variance in population estimate (that is, "index" corrected by VCF). 
  #First find the variance in the mean observed across transects.
  #This has a component due to sample variance (VarIndexSing/Pair) and due to binomial variance
  #  (Area/M)*(msing/mx)*(1-p)/(M).
  #This is A4 from F&G, where F&G's y_bar is replaced with (Area/M)*(msing/mx),
  #  the ratio estimate of the transects mean. 
  #
  ss$VarYSingHat <- with(ss, { 
    VarIndexSing + (Area/M)*(msing/mx)*(1-psing)/M  }) 
  ss$VarYPairHat <- with(ss, { 
    VarIndexPair + (Area/M)*(mpair/mx)*(1-ppair)/M })
  ss$VarYSmFl2Hat <- with(ss, { 
    VarIndexSmFl2 + (Area/M)*(msmfl2/mx)*(1-ppair)/M })
  ss$VarYSmFl3Hat <- with(ss, { 
    VarIndexSmFl3 + (Area/M)*(msmfl3/mx)*(1-psmfl)/M })
  ss$VarYSmFl4Hat <- with(ss, { 
    VarIndexSmFl4 + (Area/M)*(msmfl4/mx)*(1-psmfl)/M })
  #Now sum over strata, here observer and strata, as in F&G equation A3
  #p is shared across observers and all; here assume independent estimates for singles and pairs
  # no sampling covariance
  #need to transform data to long format (couldn't think of a better way)
  Singles <- as.data.frame(ss)[,c("Year", "strata", "obs", "m", "M", "Area", "xVar", "sCov", "sVar", "msing", "mx", "psing", "psesing", "VarYSingHat")]
  Pairs <- as.data.frame(ss)[,c("Year", "strata", "obs", "m", "M", "Area", "xVar", "pCov", "pVar", "mpair", "mx", "ppair", "psepair", "VarYPairHat")]
  SmFls2 <- as.data.frame(ss)[,c("Year", "strata", "obs", "m", "M", "Area", "xVar", "f2Cov", "f2Var", "msmfl2", "mx", "ppair", "psepair", "VarYSmFl2Hat")]
  SmFls3 <- as.data.frame(ss)[,c("Year", "strata", "obs", "m", "M", "Area", "xVar", "f3Cov", "f3Var", "msmfl3", "mx", "psmfl", "psesmfl", "VarYSmFl3Hat")]
  SmFls4 <- as.data.frame(ss)[,c("Year", "strata", "obs", "m", "M", "Area", "xVar", "f4Cov", "f4Var", "msmfl4", "mx", "psmfl", "psesmfl", "VarYSmFl4Hat")]
  
  #rename VarYSingHat and VarYPairHat to VarYHat so we can stack
  names(Singles) <- names(Pairs) <-  names(SmFls2) <- names(SmFls3) <- names(SmFls4) <- 
    c("Year", "strata", "obs", "m", "M", "Area", "xVar", "yxCov", "yVar", "my", "mx", "p", "pse", "VarYHat")
  ssLong <- rbind(Singles, Pairs, SmFls2, SmFls3, SmFls4)
  #write as function todo F&G A3 calculation
  #summing over strata that share a detection (here strata, observers, but not singles and pairs)
  VarFG <- function(data=NA, VisCorFact=NA, VisCorFactSE=NA){
    with(data, {
      SumAy <- aggregate( Area*(my/mx), by=list(Year, obs), sum) #Area*(my/mx) is the ratio estimate of the mean
      SumA2vy <- aggregate( VarYHat*(M^2), by=list(Year, obs), sum) #Area*(my/mx) = (my/mx)*(Area/M)*M = y_bar*M
      
      data.frame(Year=SumAy[,1], obs=SumAy[,2], Var=SumAy$x^2 * (VisCorFactSE^2) + (VisCorFact^2) * SumA2vy$x - (VisCorFactSE^2)*SumA2vy$x) 
    })
  }
  
  #find F&G variance of singles and pairs
  #use Delta approximation to variance of 1/p
  #do singles
  VarSing <- VarFG(data=Singles, VisCorFact=1/pData[pData$strata=="Singles", "p"],
                  VisCorFactSE = pData[pData$strata=="Singles", "se"]/(pData[pData$strata=="Singles", "p"])^2)
  VarSing <- aggregate(Var  ~ Year, data=VarSing, FUN=sum)[,2]/(numobs^2)
  #do Pairs
  VarPair <- VarFG(data=Pairs, VisCorFact=1/pData[pData$strata=="Pairs", "p"],
                  VisCorFactSE = pData[pData$strata=="Pairs", "se"]/(pData[pData$strata=="Pairs", "p"])^2)
  VarPair <- aggregate(Var  ~ Year, data=VarPair, FUN=sum)[,2]/(numobs^2)
  #do SmFls2
  VarSmFl2 <- VarFG(data=SmFls2, VisCorFact=1/pData[pData$strata=="Pairs", "p"],
                    VisCorFactSE = pData[pData$strata=="Pairs", "se"]/(pData[pData$strata=="Pairs", "p"])^2)
  VarSmFl2 <- aggregate(Var  ~ Year, data=VarSmFl2, FUN=sum)[,2]/(numobs^2)
  #do SmFls3
  VarSmFl3 <- VarFG(data=SmFls3, VisCorFact=1/pData[pData$strata=="SmFlocks", "p"],
                   VisCorFactSE = pData[pData$strata=="SmFlocks", "se"]/(pData[pData$strata=="SmFlocks", "p"])^2)
  VarSmFl3 <- aggregate(Var  ~ Year, data=VarSmFl3, FUN=sum)[,2]/(numobs^2)
  #do SmFls4
  VarSmFl4 <- VarFG(data=SmFls4, VisCorFact=1/pData[pData$strata=="SmFlocks", "p"],
                    VisCorFactSE = pData[pData$strata=="SmFlocks", "se"]/(pData[pData$strata=="SmFlocks", "p"])^2)
  VarSmFl4 <- aggregate(Var  ~ Year, data=VarSmFl4, FUN=sum)[,2]/(numobs^2)
  #sum and convert to IBB
  VarTotal <- 4*( VarSing + VarPair + 4 * VarSmFl3 + 9 * VarSmFl3 + 16 * VarSmFl4 )
  plot(Year, Nibb, pch=16, type='b', ylim=c(0, max(Nibb)+2000))
  lines(Year, Yibb, pch=22, type='b')
  arrows(x0=Year, x1=Year, y0=Nibb-2*sqrt(VarTotal), y1=Nibb+2*sqrt(VarTotal), length=0)
  #End population variance
  #########################
  #########################
  
  #########################
  #Plot the results
  plotData1 <- data.frame(Year=Year+0.1, type=rep("Population", length(Year)), 
                          Ibb=Nibb, 
                          lower=ifelse(Nibb-2*sqrt(VarTotal)<0,0,Nibb-2*sqrt(VarTotal)), 
                          upper=Nibb+2*sqrt(VarTotal)) 
  
  plotData2 <- data.frame(Year=Year-0.1, type=rep("Index", length(Year)), 
                          Ibb=Yibb,
                          lower=ifelse(Yibb-2*sqrt(VarIndex)<0,0,Yibb-2*sqrt(VarIndex)),
                          upper=Yibb+2*sqrt(VarIndex))
  
  plotData <- rbind(plotData1, plotData2)
  gplot <- ggplot() +
    geom_point(data = plotData, aes(x=Year, y=Ibb, color=type), size=2) +
    labs(title="", x="Year", y = "Indicated Breeding Birds") +
    scale_x_continuous(breaks = c(seq(1986, 2020, by=2)), limits = c(1986,2020)) +
   # scale_y_continuous(breaks = c(seq(0,30000,2000)), limits = c(0,30000)) +
    geom_linerange(data=plotData, aes(x=Year, ymin=lower, ymax=upper, color=type), size=1.1) + 
    scale_colour_manual(values=c("black", "gray50")) + 
    theme(legend.position=c(0.2, 0.75), legend.title = element_blank())
  
  return(list(popest=data.frame(Year=Year, Nibb=Nibb, seNibb=sqrt(VarTotal), Index=Yibb, seIndex=sqrt(VarIndex)), 
                           plot=gplot)
  )
}