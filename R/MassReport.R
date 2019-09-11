





MassReport=function(folder.path=NA, area=NA, method="both"){

#specify folder

if(area=="YKD"){folder.path="Q:/Waterfowl/YKD_Coastal/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="ACP"){folder.path="Q:/Waterfowl/ACP_Survey/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="CRD"){folder.path="Q:/Waterfowl/CRD_Survey/Data/Raw_Survey_Data/Observer_Transcribed_Data/"}
if(area=="WBPHS"){folder.path="Q:/Waterfowl/WBPHS/Data/Observer Transcribed Data/Processed/"}


#get all files

file.set=list.files(folder.path)

#remove anything that isn't csv

file.set=file.set[tools::file_ext(file.set)=="csv"]

#file.set=file.set[substr(file.set,1,3)=="YKG"]

write.table(file.set, paste(folder.path, "filelist.csv", sep=""), quote=F, row.names = F, col.names = F, sep="," )

for (i in 1:length(file.set)){

  path.name=paste(folder.path, file.set[i], sep='')

  if(method=="both" || method=="greenlight"){GreenLight(path.name, area=area, raw2analysis = FALSE, report=TRUE)}
  if(method=="both" || method=="data"){GreenLight(path.name, area=area, raw2analysis = TRUE, report=FALSE)}
}


}






MassPool=function(){

  PoolData(year=2001, area="YKD")
  PoolData(year=1986, area="YKG")
  PoolData(year=1989, area="YKG")
  PoolData(year=1997, area="YKG")
  PoolData(year=2005, area="YKG")
  PoolData(year=1988, area="CRD")
  PoolData(year=1998, area="CRD")
  PoolData(year=2010, area="ACP")


}
