


DubMatch = function(front.data=NA, rear.data=NA, combined.data=NA, front.obs, rear.obs, time=6, open=5){

#check if combined data was given, if not, read in each file

if(is.na(combined.data)){

  f=read.csv(front.data, header=TRUE, stringsAsFactors = FALSE)
  r=read.csv(rear.data, header=TRUE, stringsAsFactors = FALSE)


  f$matched=0
  r$matched=0

} else {

  data=read.csv(combined.data, header=TRUE, stringsAsFactors = FALSE)

  f=data[data$obs==front.obs, ]
  r=data[data$obs==rear.obs, ]


  f$matched=0
  r$matched=0
}


f.tran=unique(f$tran)
r.tran=unique(r$tran)

common=as.character(f.tran[!is.na(match(f.tran, r.tran))])

f=f[f$tran %in% common,]
r=r[r$tran %in% common,]

  #empty data frame to populate with matches

 matches=data.frame(yr=integer(),
                    tran=character(),
                    ch=character(),
                    sppn=character(),
                    grp=integer(),
                    unit=character(),
                    front=character(),
                    rear=character(),
                    crew=character(), stringsAsFactors = FALSE
                    )


for (i in 1:length(f$yr)){


 # matches=rbind(matches, c(f$yr[i], f$tran[i], "10", f$sppn[i], f$grp[i], f$unit[i], f$obs[i], r$obs[1], paste(f$obs[i], r$obs[1])))

  for (j in 1:length(r$yr)){

  if(r$matched[j]==1) {next}

  if(r$yr[j]!=f$yr[i]) {next}

  if(r$tran[j]!=f$tran[i]) {next}

  if(r$sppn[j]!=f$sppn[i]) {next}

  if(r$unit[j]!=f$unit[i]) {next}

  if(abs(f$ctime[i]-r$ctime[j])<=time){

    newline=data.frame(yr=f$yr[i],
                       tran=f$tran[i],
                       ch="11",
                       sppn=f$sppn[i],
                       grp=(f$grp[i]+r$grp[j])/2,
                       unit=f$unit[i],
                       front=f$obs[i],
                       rear=r$obs[j],
                       crew=paste(f$obs[i], r$obs[j], sep=""), stringsAsFactors = FALSE
    )

    matches=rbind(matches, newline)



    #matches= rbind(matches, list(f$yr[i], f$tran[i], "11", f$sppn[i], (f$grp[i]+r$grp[j])/2, f$unit[i], f$obs[i], r$obs[j], paste(f$obs[i], r$obs[j])))

    f$matched[i]=1
    r$matched[j]=1
    }

  }

  if (f$matched[i]==0){


    newline=data.frame(yr=f$yr[i],
                       tran=f$tran[i],
                       ch="10",
                       sppn=f$sppn[i],
                       grp=f$grp[i],
                       unit=f$unit[i],
                       front=f$obs[i],
                       rear=r$obs[1],
                       crew=paste(f$obs[i], r$obs[1], sep=""), stringsAsFactors = FALSE
    )

    matches=rbind(matches, newline)
    }

}


 for (k in 1:length(r$yr)){

   if(r$matched[k]==1) {next}


   newline=data.frame(yr=r$yr[k],
                      tran=r$tran[k],
                      ch="01",
                      sppn=r$sppn[k],
                      grp=r$grp[k],
                      unit=r$unit[k],
                      front=f$obs[1],
                      rear=r$obs[k],
                      crew=paste(f$obs[1], r$obs[k], sep=""), stringsAsFactors = FALSE
   )

   matches=rbind(matches, newline)


   #matches=rbind(matches, c(r$yr[k], r$tran[k], "01", r$sppn[k], r$grp[k], r$unit[k], f$obs[1], r$obs[k], paste(f$obs[1], r$obs[k])))


 }




return(matches)


}
