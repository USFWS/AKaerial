
#' Deprecated function to calculate index estimates for a given year of WBPHS data
#'
#' EstimatesTableWBPHS will load a given year of greenlighted data and calculate an index estimate using a ratio estimator.  This function is deprecated and replaced by WBPHStidy.
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
