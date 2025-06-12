#' Filter the data objects down into an object to pass to TidyDensities
#'
#' AnalysisFrame will compile all data for a survey-year combination in the format TidyDensities uses
#'
#' AnalysisFrame is meant to make extracting a clean data object for TidyDensities easier
#' without the need for external files.  The MasterObs, MasterTransect, MasterStrata, MasterSummary, and MasterEffort
#' data frames must be up to date.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param area The abbreviation for the project.
#' @param year The year to run.
#'
#' @return A list of 3 estimates tables matching ACPHistoric, YKGHistoric, YKDHistoric, or CRDHistoric
#'
#' @export
AnalysisFrame=function(survey, year){

  obs=MasterObs %>% filter(Survey == survey & Year == year)
  flight=MasterEffort %>% filter(Survey == survey & Year == year)
  transect=MasterSummary %>% filter(Survey %in% survey & Year == year)
  strata=MasterStrata %>% filter(Survey == survey)
  design=MasterTransect %>% filter(Survey == survey & Year == year)

  data=list(obs=obs,
                  flight=flight,
                  transect=transect,
                  strata=strata,
                  design=design)

  return(data)
}

