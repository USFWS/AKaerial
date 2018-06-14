


DubMatch = function(front.data=NA, rear.data=NA, combined.data=NA, front.obs, rear.obs, time=6, open=5){

#check if combined data was given, if not, read in each file

if(is.na(combined.data)){

  f=read.csv(front.data, header=TRUE)
  r=read.csv(rear.data, header=TRUE)

  f$matched=0
  r$matched=0

} else {

  data=read.csv(combined.data, header=TRUE)

  f=data[data$obs==front.obs, ]
  r=data[data$obs==rear.obs, ]

  f$matched=0
  r$matched=0
}


  #empty data frame to populate with matches

 matches=data.frame(yr=integer(),
                    tran=character(),
                    ch=character(),
                    sppn=character(),
                    grp=integer(),
                    unit=character(),
                    front=character(),
                    rear=character(),
                    crew=character()
                    )


for (i in 1:length(f$yr)){

  matches=rbind(matches, c(f$yr[i], f$tran[i], "10", f$sppn[i], f$grp[i], f$unit[i], f$obs[i], r$obs[1], paste(f$obs[i], r$obs[1])))

  for (j in 1:length(r$yr)){

  if(r$matched[j]==1) {next}

  if(r$yr[j]!=f$yr[i]) {next}

  if(r$tran[j]!=f$tran[i]) {next}

  if(r$sppn[j]!=f$sppn[i]) {next}

  if(r$unit[j]!=f$unit[i]) {next}

  if(abs(f$ctime-r$ctime)<=time){

    matches= rbind(matches, c(f$yr[i], f$tran[i], "11", f$sppn[i], (f$grp[i]+r$grp[j])/2, f$unit[i], f$obs[i], r$obs[j], paste(f$obs[i], r$obs[j])))

    f$matched[i]=1
    r$matched[j]=1
    }

  }

  if (f$matched[i]==0){

    matches=rbind(matches, c(f$yr[i], f$tran[i], "10", f$sppn[i], f$grp[i], f$unit[i], f$obs[i], r$obs[1], paste(f$obs[i], r$obs[1])))

    }

}


 for (k in 1:length(f$yr)){

   if(f$matched[k]==1) {next}

   matches=rbind(matches, c(r$yr[k], r$tran[k], "01", r$sppn[k], r$grp[k], r$unit[k], f$obs[1], r$obs[k], paste(f$obs[1], r$obs[k])))


 }




return(matches)


}
