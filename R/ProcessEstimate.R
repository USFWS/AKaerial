#' Process a raw estimate into a useful table
#'
#' ProcessEstimate will take an AnalysisFrame object and summarize it into estimates
#'
#' ProcessEstimate will take an AnalysisFrame object and summarize it into estimates. The
#' usual output is a simple observer-level file, but it gets translated into observer-, strata-,
#' and combined estimates.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data AnalysisFrame output object consisting of summary information
#'
#' @return A list of combined, expanded.table, and output.table data frames
#'
#' @export
ProcessEstimate=function(data){

  output.table=data$estimates
  expanded.table=data$counts.final
  combined=CombineEstimates(output.table)

  output.table = output.table %>% filter(!(Species %in% c("START", "END")))
  expanded.table = expanded.table %>% filter(!(Species %in% c("START", "END")))
  combined = combined %>% filter(!(Species %in% c("START", "END")))

  return(list(output.table=output.table, expanded.table=expanded.table, combined=combined))

}
