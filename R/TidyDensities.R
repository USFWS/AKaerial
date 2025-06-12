#' Tidy standard ratio estimator for aerial survey data
#'
#' TidyDensities will combine spatially-referenced observations with design transects and strata to create an index estimate
#'
#' TidyDensities is the primary function in AKaerial for producing index estimates. It will take an object from DataSelect and adjust counts, summarize
#' spatial information, and calculate indices and their associated standard errors.  Also retained are estimates of densities of bird by strata and on
#'  each transect.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param data The DataProcess list object to be analyzed
#' @param area The area code for dedicated MBM Alaska region surveys.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKDV - Yukon Kuskokwim Delta, 2018 visibility study
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item KIG - Kigigak Island
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  \item BLSC - 2018 Black Scoter Survey
#'  }
#' @param output TRUE/FALSE Should results be written to text files in the current working directory?
#'
#' @return List object with 2 elements: \enumerate{
#' \item estimates Observer-specific estimates for the species indicated in the sppntable estimates column
#' \item counts.final Deeper level count information by transect, strata, species, and observer
#' }
#'
#' @export
TidyDensities=function(data, area, output=FALSE) {

  #Save old warning settings to revert to before leaving function
  #oldw <- getOption("warn")
  #Suppress spatial warnings inside function call
  #options(warn=-1)


  if(area=="YKDV"){area="YKD"}

  #Sum the counts by combinations of species/transect

  counts.t = data$transect %>%
    dplyr::group_by(Year, Observer, Species, ctran, area, strata) %>%
    dplyr::summarise(Num = sum(Num),
                     total = sum(total),
                     itotal = sum(itotal),
                     ibb = sum(ibb),
                     sing1pair2 = sum(sing1pair2),
                     flock = sum(flock)) %>%
    tidyr::drop_na()


  strata.level = counts.t %>%
    dplyr::group_by(Year, Observer, Species, strata) %>%
    dplyr::summarise(total.area=sum(area),
                     total.area.var=var(area),
                     total.s=sum(total),
                     total.v=var(total),
                     total.cov=cov(total,area),
                     density.total=total.s/total.area,
                     itotal.s=sum(itotal),
                     itotal.v=var(itotal),
                     itotal.cov=cov(itotal,area),
                     density.itotal=itotal.s/total.area,
                     ibb.s=sum(ibb),
                     ibb.v=var(ibb),
                     ibb.cov=cov(ibb, area),
                     density.ibb=ibb.s/total.area,
                     sing1pair2.s=sum(sing1pair2),
                     sing1pair2.v=var(sing1pair2),
                     sing1pair2.cov=cov(sing1pair2, area),
                     density.sing1pair2=sing1pair2.s/total.area,
                     flock.s=sum(flock),
                     flock.v=var(flock),
                     flock.cov=cov(flock, area),
                     density.flock=flock.s/total.area) %>%
    merge(., data$strata %>% select(strata, layer.area, M), by="strata") %>%
    dplyr::mutate(total.est = density.total*layer.area,
                     itotal.est= density.itotal*layer.area,
                     ibbtotal.est = density.ibb*layer.area,
                     sing1pair2.est = density.sing1pair2*layer.area,
                     flock.est = density.flock*layer.area) %>%
    filter(!(strata %in% c("Nonhabitat", "Mountains", "Water", "0", "Zero", "zero")))

  estimates= strata.level %>%
    dplyr::group_by(Year, Observer, Species) %>%
    dplyr::summarise(total.est=sum(total.est),
                     itotal.est=sum(itotal.est),
                     ibbtotal.est=sum(ibbtotal.est),
                     sing1pair2.est=sum(sing1pair2.est),
                     flock.est=sum(flock.est))


  ### Var(N) ###
  reps2=counts.t %>%
    dplyr::group_by(Year, Observer, strata) %>%
    dplyr::summarise(m=length(unique(ctran)))


  data$strata=merge(data$strata, reps2, by="strata")

  strata.level=merge(strata.level, data$strata %>% dplyr::select(strata, Observer, m), by=c("strata", "Observer"))


  #See equation 12.9, p. 249 in "Analysis and Management of Animal Populations"
  #Williams, Nichols, Conroy; 2002

  strata.level = units::drop_units(strata.level) %>%
    mutate(prop.m = ((1-(m/M))/m),
        var.N=(M^2)*prop.m*(total.v+(density.total^2)*(total.area.var)-(2*density.total*total.cov)),
        var.Ni=(M^2)*prop.m*(itotal.v+(density.itotal^2)*(total.area.var)-(2*density.itotal*itotal.cov)),
        var.Nib=(M^2)*prop.m*(ibb.v+(density.ibb^2)*(total.area.var)-(2*density.ibb*ibb.cov)),
        var.Nsing1pair2=(M^2)*prop.m*(sing1pair2.v+(density.sing1pair2^2)*(total.area.var)-(2*density.sing1pair2*sing1pair2.cov)),
        var.Nflock=(M^2)*prop.m*(flock.v+(density.flock^2)*(total.area.var)-(2*density.flock*flock.cov))
    )


  var.est = strata.level %>%
    dplyr::group_by(Year, Observer, Species) %>%
    dplyr::summarise(var.N = sum(var.N),
                     var.Ni = sum(var.Ni),
                     var.Nib = sum(var.Nib),
                     var.Nsing1pair2 = sum(var.Nsing1pair2),
                     var.Nflock = sum(var.Nflock),
                     SE = sqrt(var.N),
                     SE.i = sqrt(var.Ni),
                     SE.ibb = sqrt(var.Nib),
                     SE.sing1pair2 = sqrt(var.Nsing1pair2),
                     SE.flock = sqrt(var.Nflock))

  estimates=merge(estimates, var.est, by=c("Year", "Observer", "Species"), all=TRUE)


  #Output tables to txt files if requested

  if(output==TRUE){
    write.table(counts.final, file="finalcounts.txt", quote=FALSE, row.names=FALSE)
    write.table(estimates, file="estimates.txt", quote=FALSE, row.names=FALSE)

  }

  return(list("estimates"=estimates, "counts.final"=strata.level))
}
