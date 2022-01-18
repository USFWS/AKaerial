#' Title
#' @param dat A data frame containing transect-level sums of observed singles and pairs, see details. 
#' @param vcfData A data frame containing visibility correction factors by strata, see details. 
#' @param M A named vector giving the total number of possible transects in each strata. Names must match strata names in data. 
#' @param prob A logical indicating where to do the calculation based on detection probability or it reciprocal, vcf. 
#' @return A list containing population estimates of 'indicated breeding birds', the 'index" estimates (without detection correction), standard errors of each, and a ggplot objects of each by year.
#' @details 
#'
#' @examples
popest <- function(dat=NA, vcfData=NA, Mdat=NA, prob=FALSE){
  #Code to calculate detection-corrected population estimates from stratified plot data 
  # using a ratio estimator. The approach is from Fieberg and Giudice (2008, JWM 72:837), 
  # hereafter F&G. 
  #The key parts are equations A3 and A4 of F&G.  A3 is explained in F&G; 
  # A4 is explained best in the text book of Thompson (2012, Sampling).  
  #Key difference from above citation is the use of the ratio estimator 
  #for the mean number observed across transects.  This was not in F&G or Thompson.
  
  #Code originally written by Erik Osnas, December 2018. Modified in January & February 2019.
  #Modified 20190925 for data request from Kylee Durham.
  # write as function that accepts dataframe of transect level summaries, and output VCF corrected population estimates.
  #Copyright:  GNU General Public License v3.0
  library(ggplot2)
  library(dplyr)
  
  dat <- left_join(dat, vcfData[,1:2])
  sing <- dat[dat$Obs_Type=="single", ] 
  names(sing)[6] <- "Singles"
  pair <- dat[dat$Obs_Type=="pair", ]
  names(pair)[6] <- "Pairs"
  dat2 <- data.frame(sing[,-c(4,6)], Singles=sing[,"Singles"], Pairs=pair[,"Pairs"])
  
  #flkdrakes are treated as singles. 
  #singles, pairs, and flkdrake of <=4 are doubled for indicated breeding birds 
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
  
  dat2$Singles <- dat2$Singles + dat2$fdop1 + 2*dat2$fdop2 + 3*dat2$fdopsf3 + 4*dat2$fdopsf4
  dat2$Pairs <- dat2$Pairs 
  dat <- dat2
  rm(dat2)
  
  ########################
  #find mean by statum and year
  msing <- aggregate(dat$Singles, by=list(strata=dat$strata, Year=dat$Year), mean)
  mpair <- aggregate(dat$Pairs, by=list(strata=dat$strata, Year=dat$Year), mean)
  mx <- aggregate(dat$obs.area, by=list(strata=dat$strata, Year=dat$Year), mean) #, na.rm=TRUE 
  Area <- aggregate(dat$strata.area, by=list(strata=dat$strata, Year=dat$Year), mean)
  Year=unique(dat$Year)
  mvcf <- aggregate(dat$vcf, by=list(strata=dat$strata, Year=dat$Year), mean)
  
  #calculate "index" estimate, sum singles and pairs over strata by year
  #Area*msing/mx is the ratio estimate of the population total of observed singles and pairs
  # Yibb is twice the number of singles and pairs
  Ysing <- aggregate(Area$x*msing$x/mx$x, list(Year=mx$Year), sum)
  Ypair <- aggregate(Area$x*mpair$x/mx$x, list(Year=mx$Year), sum)
  Yibb <- 2*(Ysing$x + Ypair$x)
  
  #Find population estimate by applying VCF correction
  #For each strata ain each year, multiply by VCF before summing over strata
  Nsing <- aggregate(mvcf$x*Area$x*msing$x/mx$x, 
                     list(Year=mx$Year), sum)
  Npair <- aggregate(mvcf$x*Area$x*mpair$x/mx$x, 
                     list(Year=mx$Year), sum)
  Nibb <- 2*(Nsing$x + Npair$x)
  ###############################################################################
  #Find variance of estimates
  
  #put this all together in a list so we can interate through by strata and year 
  #to find variance and covariance between counts and transect areas
  ss <- list(Year=msing$Year, strata=msing$strata, m = c(), M=c(), Area=Area$x, xVar=c(), sCov = c(), 
             sVar=c(), pVar=c(), pCov=c(), msing=msing$x, mpair=mpair$x, mx=mx$x, vcf=c())
  for(i in 1:length(msing$x)){
    tempdat <- dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                   c("obs.area", "Singles")]
    ss$m[i] <- dim(tempdat)[1]
    ss$M[i] <- Mdat[Mdat$Year== msing$Year[i] & Mdat$strata==msing$strata[i], "M"]
    temp <- cov(as.matrix(tempdat),  use="na.or.complete")
    ss$xVar[i] <- temp[1,1]
    ss$sCov[i] <- temp[1,2]
    ss$sVar[i] <- temp[2,2]
    temp <- cov(as.matrix(dat[c(dat$Year== msing$Year[i] & dat$strata==msing$strata[i]), 
                              c("obs.area", "Pairs")]), use="na.or.complete")
    ss$pCov[i] <- temp[1,2]
    ss$pVar[i] <- temp[2,2]
    
    ss$vcf[i] = vcfData[vcfData$strata==msing$strata[i],"vcf"]
    ss$vcfse[i] = vcfData[vcfData$strata==msing$strata[i],"se"]
  }
  
  #########################
  #Calculate index variance
  #first sample variance of the mean number observed per transect.
  #The sample variance is eq. 7.5 from Thompson 2012.
  ss$VarIndexSing <- with(ss, { 
    (1 - m/M)*(sVar + (msing/mx)^2*xVar - 2*(msing/mx)*sCov)/m  }) 
  ss$VarIndexPair <- with(ss, { 
    (1 - m/M)*(pVar + (mpair/mx)^2*xVar - 2*(mpair/mx)*pCov)/m  })
  #expand to all strata area; sum singles and pairs; convert to IBB
  ss$VarExpandIBB <- 4 * ss$M^2 * (ss$VarIndexSing + ss$VarIndexPair)

  VarIndex <- aggregate(VarExpandIBB  ~ Year, data=as.data.frame(ss), FUN=sum)[,2]
  #End index variance 
  #########################
  #########################
  #Calculate variance in population estimate (that is, "index" corrected by VCF). 
  #First find the variance in the mean observed across transects.
  #This has a component due to sample variance (VarIndexSing/Pair) and due to binomial variance
  #  (Area/M)*(msing/mx)*(vcf-1)/(vcf*M).
  #This is A4 from F&G, where F&G's y_bar is replaced with (Area/M)*(msing/mx),
  #  the ratio estimate of the transects mean. 
  #The reciprocal of detection (VCF) is used. 
  ss$VarYSingHat <- with(ss, { 
    VarIndexSing + (Area/M)*(msing/mx)*(vcf-1)/(vcf*M)  }) 
  ss$VarYPairHat <- with(ss, { 
    VarIndexPair + (Area/M)*(mpair/mx)*(vcf-1)/(vcf*M) })
  #Now sum over strata as in F&G equation A3
  #VCF is shared for low, LowE, LowN, and LowS; independent estimates for other strata
  #VCF is also shared between singles and pairs.  
  #need to transform data to long format (couldn't think of a better way)
  Singles <- as.data.frame(ss)[,c("Year", "strata", "m", "M", "Area", "xVar", "sCov", "sVar", "msing", "mx", "vcf", "vcfse", "VarYSingHat")]
  Pairs <- as.data.frame(ss)[,c("Year", "strata", "m", "M", "Area", "xVar", "pCov", "pVar", "mpair", "mx", "vcf", "vcfse", "VarYPairHat")]
  #rename VarYSingHat and VarYPairHat to VarYHat so we can stack
  names(Singles) <- names(Pairs) <- 
    c("Year", "strata", "m", "M", "Area", "xVar", "yxCov", "yVar", "my", "mx", "vcf", "vcfse", "VarYHat")
  ssLong <- rbind(Singles, Pairs)
  #write as function todo F&G A3 calculation
  #summing over strata that share a VCF (here density strata and singles and pairs)
  VarFG <- function(data=NA, VisCorFact=NA, VisCorFactSE=NA){
    with(data, {
      SumAy <- aggregate( Area*(my/mx), by=list(Year), sum) #Area*(my/mx) is the ratio estimate of the mean
      SumA2vy <- aggregate( VarYHat*(M^2), by=list(Year), sum) #Area*(my/mx) = (my/mx)*(Area/M)*M = y_bar*M
      
      data.frame(Year=SumAy[,1], Var=SumAy$x^2 * (VisCorFactSE^2) + (VisCorFact^2) * SumA2vy$x - (VisCorFactSE^2)*SumA2vy$x) 
    })
  }
  
  #select the four low density strata and common VCF
  VarLow <- VarFG(data=as.list(as.data.frame(ssLong)[ssLong$strata%in%c("Low", "LowN", "LowS", "LowE"),]), 
                  VisCorFact=vcfData[vcfData$strata=="Low", "vcf"],
                  VisCorFactSE = vcfData[vcfData$strata=="Low", "se"])
  names(VarLow)[2] <- "VarLow"
  #Medium strata
  VarMed <- VarFG(data=as.list(as.data.frame(ssLong)[ssLong$strata=="Medium",]), 
                  VisCorFact=vcfData[vcfData$strata=="Medium", "vcf"],
                  VisCorFactSE = vcfData[vcfData$strata=="Medium", "se"])
  names(VarMed)[2] <- "VarMed"
  #High strata
  VarHigh <- VarFG(data=as.list(as.data.frame(ssLong)[ssLong$strata=="High",]), 
                   VisCorFact=vcfData[vcfData$strata=="High", "vcf"],
                   VisCorFactSE = vcfData[vcfData$strata=="High", "se"])
  names(VarHigh)[2] <- "VarHigh"
  #sum and convert to IBB
  VarData <- left_join(left_join(left_join(data.frame(Year), VarLow), VarMed), VarHigh)
  VarTotal <- 4*rowSums(VarData[,-1], na.rm=TRUE)
  #End population variance
  #########################
  #########################
  
  #########################
  #Plot the results
  jig <- 0.1
  plotData1 <- data.frame(Year=Year+jig, type=rep("Density-adjusted VCF", length(Year)), 
                          Ibb=Nibb, 
                          lower=ifelse(Nibb-2*sqrt(VarTotal)<0,0,Nibb-2*sqrt(VarTotal)), 
                          upper=Nibb+2*sqrt(VarTotal)) 
  
  plotData2 <- data.frame(Year=Year-jig, type=rep("Index", length(Year)), 
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