

#specify folder

folder.path="Q:/Waterfowl/YKD_Coastal/Data/Raw_Survey_Data/Observer_Transcribed_Data/"
#folder.path="Q:/Waterfowl/ACP_Survey/Data/Observer Transcribed Data/"
#folder.path="L:/Observer Transcribed Data/"
#folder.path="Q:/Waterfowl/BLSC Survey/Data/QC Transcribed Data/"


#get all files

file.set=list.files(folder.path)

#remove anything that isn't csv

file.set=file.set[tools::file_ext(file.set)=="csv"]

write.table( file.set, paste(folder.path, "filelist.csv", sep=""), quote=F, row.names = F, col.names = F, sep="," )

for (i in 1:length(file.set)){

  path.name=paste(folder.path, file.set[i], sep='')

  GreenLight(path.name, area="YKD")

}
