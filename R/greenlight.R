

GreenLight=function(path.name, area){



  type <- file_ext(path.name)

  if(type == "txt") {data <- read.table(path.name, header=TRUE)}
  if(type == "csv") {data <- read.csv(path.name, header=TRUE)}

  if(!area %in% c("ACP", "YKD", "YKG", "CRD", "BPOP")){

    print("Area not supported or incorrect.  Currently supported areas are ACP, YKD, YKG, CRD, BPOP.")
    break
  }

  latlong=SpatialNA(dat1,method="greenlight")


  if(is.na(latlong)){
  n.latlong=0
  s.latlong="green"
  }
  else {
  n.latlong=length(latlong[,1])
  s.latlong="yellow"
  }



  rmd.path=system.file("rmd/greenlight.Rmd", package="AKaerial")

  render(rmd.path, output_dir=getwd())

}




ColMatch=function(data){

  necessary=c("yr", "se", "obs", "strat", "tran", "lat", "long", "sppn", "grp", "unit")
  return(all(necessary %in% colnames(data)))

}
