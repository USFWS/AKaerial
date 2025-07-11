#' Recreate the entire ACPHistoric, YKDHistoric, YKGHistoric, and CRDHistoric estimates objects
#'
#' FullHistoric will re-run the entire estimates process for all years
#'
#' FullHistoric will take the current data objects providing strata, transect, and observation
#' information and run the entire historic estimates process over all years for ACP, YKD, YKG, and CRD.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @return 4 lists of 3 estimates tables matching ACPHistoric, YKGHistoric, YKDHistoric, and CRDHistoric
#'
#' @export
FullHistoric=function(){

  years=unique(MasterObs$Year[MasterObs$Survey=="YKD"])

  for(i in 1:length(years)){
    data=AnalysisFrame(survey="YKD", year=years[i])
    est=TidyDensities(data=data, area="YKD")
    this.combined=CombineEstimates(est$estimates)

    if(i==1){output.table=est$estimates
    expanded.table=est$counts.final
    combined=this.combined}
    if(i>1){output.table=rbind(output.table, est$estimates)
    expanded.table=rbind(expanded.table, est$counts.final)
    combined=rbind(combined, this.combined)}

  }

  output.table$area="YKD"
  expanded.table$area="YKD"
  combined$area="YKD"

  YKD=FinishEstimate(output.table, expanded.table, combined)

  years=unique(MasterObs$Year[MasterObs$Survey=="YKG"])

  for(i in 1:length(years)){
    data=AnalysisFrame(survey="YKG", year=years[i])
    est=TidyDensities(data=data, area="YKG")
    this.combined=CombineEstimates(est$estimates)

    if(i==1){output.table=est$estimates
    expanded.table=est$counts.final
    combined=this.combined}
    if(i>1){output.table=rbind(output.table, est$estimates)
    expanded.table=rbind(expanded.table, est$counts.final)
    combined=rbind(combined, this.combined)}

  }

  output.table$area="YKG"
  expanded.table$area="YKG"
  combined$area="YKG"

  YKG=FinishEstimate(output.table, expanded.table, combined)

  years=unique(MasterObs$Year[MasterObs$Survey=="CRD"])

  for(i in 1:length(years)){
    data=AnalysisFrame(survey="CRD", year=years[i])
    est=TidyDensities(data=data, area="CRD")
    this.combined=CombineEstimates(est$estimates)
    if(i==1){output.table=est$estimates
    expanded.table=est$counts.final
    combined=this.combined
    }
    if(i>1){output.table=rbind(output.table, est$estimates)
    expanded.table=rbind(expanded.table, est$counts.final)
    combined=rbind(combined, this.combined)}

  }

  output.table$area="CRD"
  expanded.table$area="CRD"
  combined$area="CRD"

  CRD=FinishEstimate(output.table, expanded.table, combined)

  years=unique(MasterObs$Year[MasterObs$Survey=="ACP"])

  for(i in 1:length(years)){
    data=AnalysisFrame(survey="ACP", year=years[i])
    est=TidyDensities(data=data, area="ACP")
    this.combined=CombineEstimates(est$estimates)

    if(i==1){output.table=est$estimates
    expanded.table=est$counts.final
    combined=this.combined}
    if(i>1){output.table=rbind(output.table, est$estimates)
    expanded.table=rbind(expanded.table, est$counts.final)
    combined=rbind(combined, this.combined)}

  }

  output.table$area="ACP"
  expanded.table$area="ACP"
  combined$area="ACP"

  ACP=FinishEstimate(output.table, expanded.table, combined)

  save(YKG, file="C:/Users/cfrost/OneDrive - DOI/Waterfowl/ykg.rda")
  save(CRD, file="C:/Users/cfrost/OneDrive - DOI/Waterfowl/crd.rda")
  save(ACP, file="C:/Users/cfrost/OneDrive - DOI/Waterfowl/acp.rda")
  save(YKD, file="C:/Users/cfrost/OneDrive - DOI/Waterfowl/ykd.rda")

invisible()

}
