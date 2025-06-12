#' Perform final filters and additions to an estimates object by survey
#'
#' FinishEstimate will filter and add as appropriate to an estimates object
#'
#' FinishEstimate checks the appropriate species list for the final estimates object and will add
#' or remove entries as appropriate. This includes START and END.  It will also combine observer estimates
#' into one combined table where appropriate.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @return A clean estimates object
#'
#' @export
FinishEstimate=function(output.table, expanded.table, combined){

  if(combined$area[1]=="YKD"){
    years=unique(MasterObs$Year[MasterObs$Survey=="YKD"])

    sp.list = c("AMWI",
                "EUWI",
                "GWTE",
                "MALL",
                "NOPI",
                "NSHO",
                "GADW",
                "CANV",
                "REDH",
                "UNSC",
                "BLSC",
                "WWSC",
                "SUSC",
                "SCOT",
                "SPEI",
                "STEI",
                "COEI",
                "GOLD",
                "LTDU",
                "BUFF",
                "RBME",
                "COME",
                "UNME",
                "RNDU",
                "COLO",
                "PALO",
                "RTLO",
                "UNLO",
                "RNGR",
                "HOGR",
                "UNGR",
                "JAEG",
                "ARTE",
                "GLGU",
                "MEGU",
                "SAGU",
                "CORA",
                "SEOW",
                "SNOW",
                "GOEA",
                "BAEA")
  }

  if(combined$area[1]=="YKG"){
    years=unique(MasterObs$Year[MasterObs$Survey=="YKG"])

    sp.list=c("EMGO",
              "GWFG",
              "BRAN",
              "CCGO",
              "TAVS",
              "SWAN",
              "SWANN",
              "SACR",
              "SNGO")
  }

  if(combined$area[1]=="ACP"){
    years=unique(MasterObs$Year[MasterObs$Survey=="ACP"])

    sp.list=c("AMWI",
              "ARTE",
              "BLSC",
              "BRAN",
              "CANV",
              "CCGO",
              "COEI",
              "COME",
              "CORA",
              "GADW",
              "GLGU",
              "GOEA",
              "GWFG",
              "GWTE",
              "JAEG",
              "KIEI",
              "LTDU",
              "MALL",
              "NOPI",
              "NSHO",
              "PALO",
              "RBME",
              "RNGR",
              "RTLO",
              "SACR",
              "SAGU",
              "SEOW",
              "SNGO",
              "SNOW",
              "SPEI",
              "STEI",
              "SUSC",
              "SWAN",
              "SWANN",
              "UNEI",
              "UNGR",
              "UNME",
              "UNSC",
              "WWSC",
              "YBLO" )
  }


  if(combined$area[1]=="CRD"){
    years=unique(MasterObs$Year[MasterObs$Survey=="CRD"])

    sp.list=c("CCGO",
              "DCGO",
              "SWAN",
              "SWANN")
  }


  output.table = output.table %>% filter((Species %in% sp.list))
  expanded.table = expanded.table %>% filter((Species %in% sp.list))
  combined = combined %>% filter((Species %in% sp.list))

  combined = tidyr::complete(combined, Year, Species) %>%
    filter(Species %in% sp.list) %>%
    fill(area, .direction=c("updown"))

  combined[is.na(combined)]=0

  output.table = tidyr::complete(output.table, nesting(Year, Observer), Species) %>%
    filter(Species %in% sp.list) %>%
    fill(area, .direction=c("updown"))


  output.table[is.na(output.table)]=0

  colnames(output.table)[colnames(output.table)=="total.est"]="total"
  colnames(output.table)[colnames(output.table)=="itotal.est"]="itotal"
  colnames(output.table)[colnames(output.table)=="ibbtotal.est"]="ibb"
  colnames(output.table)[colnames(output.table)=="sing1pair2.est"]="sing1pair2"
  colnames(output.table)[colnames(output.table)=="flock.est"]="flock"
  colnames(output.table)[colnames(output.table)=="var.N"]="total.var"
  colnames(output.table)[colnames(output.table)=="var.Ni"]="itotal.var"
  colnames(output.table)[colnames(output.table)=="var.Nib"]="ibb.var"
  colnames(output.table)[colnames(output.table)=="var.Nsing1pair2"]="sing1pair2.var"
  colnames(output.table)[colnames(output.table)=="var.Nflock"]="flock.var"
  colnames(output.table)[colnames(output.table)=="SE"]="total.se"
  colnames(output.table)[colnames(output.table)=="SE.i"]="itotal.se"
  colnames(output.table)[colnames(output.table)=="SE.ibb"]="ibb.se"
  colnames(output.table)[colnames(output.table)=="SE.sing1pair2"]="sing1pair2.se"
  colnames(output.table)[colnames(output.table)=="SE.flock"]="flock.se"

  output.table = output.table %>% select(Year,
                                         Observer,
                                         Species,
                                         total,
                                         total.var,
                                         total.se,
                                         itotal,
                                         itotal.var,
                                         itotal.se,
                                         ibb,
                                         ibb.var,
                                         ibb.se,
                                         sing1pair2,
                                         sing1pair2.var,
                                         sing1pair2.se,
                                         flock,
                                         flock.var,
                                         flock.se,
                                         area)


  expanded.table = tidyr::complete(expanded.table, nesting(Year, Observer, strata), Species) %>%
    filter(Species %in% sp.list) %>%
    fill(area, total.area, total.area.var, .direction=c("updown")) %>%
    fill(M, m, prop.m, .direction=c("updown"))


  expanded.table[is.na(expanded.table)]=0

  expanded.table=expanded.table %>% complete(Year=min(expanded.table$Year):max(expanded.table$Year), strata, Species) %>%
    fill(area, .direction=c("updown"))

  output.table=output.table %>% complete(Year=min(output.table$Year):max(output.table$Year), Species) %>%
    fill(area, .direction=c("updown"))

  combined=combined %>% complete(Year=min(combined$Year):max(combined$Year), Species) %>%
    fill(area, .direction=c("updown"))

  ##BEGIN PROJECT-YEAR-SPECIFIC NA

  if(combined$area[1]=="YKG"){

    combined[combined$Year==1985 & combined$Species %in% c("BRAN", "SACR", "SNGO"), 3:17]=NA
    combined[combined$Year==1986 & combined$Species %in% c("SACR", "SNGO"), 3:17]=NA
    combined[combined$Year %in% c(1987:1999) & combined$Species == "SNGO", 3:17]=NA
    combined[combined$Year >= 2019 & combined$Species == "SWANN", 3:17]=NA

    output.table[output.table$Year==1985 & output.table$Species %in% c("BRAN", "SACR", "SNGO"), 4:18]=NA
    output.table[output.table$Year==1986 & output.table$Species %in% c("SACR", "SNGO"), 4:18]=NA
    output.table[output.table$Year %in% c(1987:1999) & output.table$Species == "SNGO", 4:18]=NA
    output.table[output.table$Year >= 2019 & output.table$Species == "SWANN", 4:18]=NA

    expanded.table[expanded.table$Year==1985 & expanded.table$Species %in% c("BRAN", "SACR", "SNGO"), 5:40]=NA
    expanded.table[expanded.table$Year==1986 & expanded.table$Species %in% c("SACR", "SNGO"), 5:40]=NA
    expanded.table[expanded.table$Year %in% c(1987:1999) & expanded.table$Species == "SNGO", 5:40]=NA
    expanded.table[expanded.table$Year >= 2019 & expanded.table$Species == "SWANN", 5:40]=NA


  }

  if(combined$area[1]=="YKD"){

    combined[combined$Year==1988 & combined$Species %in% c("COLO", "PALO", "RTLO", "UNLO", "RNGR", "HOGR", "UNGR", "JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "CORA", "SEOW", "SNOW", "GOEA", "BAEA"), 3:17]=NA
    combined[combined$Year==1989 & combined$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 3:17]=NA
    combined[combined$Year %in% c(1990,1991) & combined$Species %in% c("JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 3:17]=NA
    combined[combined$Year==1992 & combined$Species %in% c("JAEG", "GOEA", "BAEA"), 3:17]=NA
    combined[combined$Year >= 1993 & combined$Year < 2023 & combined$Species %in% c("GOEA", "BAEA"), 3:17]=NA
    combined[combined$Year >= 2017 & combined$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU"), 3:17]=NA

    output.table[output.table$Year==1988 & output.table$Species %in% c("COLO", "PALO", "RTLO", "UNLO", "RNGR", "HOGR", "UNGR", "JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "CORA", "SEOW", "SNOW", "GOEA", "BAEA"), 4:18]=NA
    output.table[output.table$Year==1989 & output.table$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 4:18]=NA
    output.table[output.table$Year %in% c(1990,1991) & output.table$Species %in% c("JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 4:18]=NA
    output.table[output.table$Year==1992 & output.table$Species %in% c("JAEG", "GOEA", "BAEA"), 4:18]=NA
    output.table[output.table$Year >= 1993 & output.table$Year < 2023 & output.table$Species %in% c("GOEA", "BAEA"), 4:18]=NA
    output.table[output.table$Year >= 2017 & output.table$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU"), 4:18]=NA

    expanded.table[expanded.table$Year==1988 & expanded.table$Species %in% c("COLO", "PALO", "RTLO", "UNLO", "RNGR", "HOGR", "UNGR", "JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "CORA", "SEOW", "SNOW", "GOEA", "BAEA"), 5:40]=NA
    expanded.table[expanded.table$Year==1989 & expanded.table$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 5:40]=NA
    expanded.table[expanded.table$Year %in% c(1990,1991) & expanded.table$Species %in% c("JAEG", "ARTE", "GLGU", "MEGU", "SAGU", "SEOW", "SNOW", "GOEA", "BAEA"), 5:40]=NA
    expanded.table[expanded.table$Year==1992 & expanded.table$Species %in% c("JAEG", "GOEA", "BAEA"), 5:40]=NA
    expanded.table[expanded.table$Year >= 1993 & expanded.table$Year < 2023 & expanded.table$Species %in% c("GOEA", "BAEA"), 5:40]=NA
    expanded.table[expanded.table$Year >= 2017 & expanded.table$Species %in% c("ARTE", "GLGU", "MEGU", "SAGU"), 5:40]=NA


  }

  return(list(output.table=output.table, expanded.table=expanded.table, combined=combined))

}
