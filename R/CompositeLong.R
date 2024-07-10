
#' Compile index estimates for Pacific Flyway data book in one long format
#'
#' CompositeLong compiles appropriate estimates from multiple surveys for Taverner's geese, Pacific whitefronts, mid-continent whitefronts, western tundra and Pacific coast trumpeter swans, and lesser Canada geese.
#'
#' USFWS-MBM-Alaska Region surveys are used in harvest management decisions by the Pacific Flyway.  Although management plans generally use a single survey index to decide
#' appropriate management actions, 6 species require a composite index across 2 or more Alaska Region surveys.  These species and their composite indices are: \enumerate{
#' \item Taverner's geese (TAVS) - All strata from the Arctic Coastal Plain (ACPHistoric), CCGO in the "low" stratum or at latitudes > 63 on the Yukon-Kuskokwim Delta (YKGHistoric), strata 10, 11, and 99 (clipped section of 9) on the WBPHS (WBPHSHistoric)
#' \item Pacific white-fronted geese - GWFG on the Yukon-Kuskokwim Delta (YKGHistoric) or in strata 8 or 99 on the WBPHS (WBPHSHistoric)
#' \item Western tundra swans (TUSW) - TUSW or SWAN on the Yukon-Kuskokwim Delta (YKGHistoric) or in strata 8, 99, 10, or 11 on the WBPHS (WBPHSHistoric)
#' \item Lesser Canada geese - CCGO in strata 1, 2, 3, 4, or 12 on the WBPHS (WBPHSHistoric)
#' \item Mid-continent white-fronted geese - GWFG in strata 3, 4, 5, 6, 10, or 11 on the WBPHS (WBPHSHistoric) and all GWFG on the Arctic Coastal Plain (ACPHistoric)
#' \item Pacific coast trumpeter swans (TRSW) - TRSW or SWAN in strata 1, 2, 3, 4, 6, or 7 on the WBPHS (WBPHSHistoric)
#' }
#' Note that WBPHS stratum 9 is clipped and renamed 99 to avoid double counting birds in an area surveyed on the Yukon-Kuskokwim Delta surveys.
#' The default range of years for each species is set to the earliest year in which all surveys that make up the index were conducted.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param tavs.year range of years for the Taverner's geese composite table, defaults to 1985-2023
#' @param pw.year range of years for the Pacific white-fronted goose composite table, defaults to 1985-2023
#' @param tusw.year range of years for the western tundra swan composite table, defaults to 1985-2023
#' @param lesser.year range of years for the lesser Canada goose composite table, defaults to 1964-2023
#' @param mcw.year range of years for the mid-continent white-fronted goose composite table, defaults to 1964-2023
#' @param trsw.year range of years for the Pacific coast trumpeter swan composite table, defaults to 1964-2023
#'
#' @return Long data frame of composite indices
#'
#' @export
CompositeLong = function(tavs.year = c(1985:2024),
                            pw.year = c(1985:2024),
                            tusw.year = c(1985:2024),
                            lesser.year = c(1964:2024),
                            mcw.year = c(1964:2024),
                            trsw.year = c(1964:2024),
                            versioning=FALSE){


  # TAVERNERS
  #
  #Estimates and se’s for 1) indicated total birds, 2) indicated singles and paired
  #
  #1)	ACP: all strata combined
  #2)	YKG: whatever you identified as Taverners (“Low”, Lat >63)
  #3)	WBPHS:
  # a)	clipped stratum 99
  # b)	stratum 10
  # c)	stratum 11
  # 4)	Total and se for all surveys combined

  acp.tavs = ACPHistoric$combined %>%
    dplyr::filter(Species == "CCGO", Year %in% tavs.year) %>%
    dplyr::select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to = "type", values_to = "estimate") %>%
    dplyr::mutate(area="ACP", Stratum="all")

  ykg.tavs = YKGHistoric$combined %>%
    dplyr::filter(Species == "TAVS", Year %in% tavs.year) %>%
    dplyr::select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to = "type", values_to = "estimate") %>%
    dplyr::mutate(area="YKG", Stratum="all")

  wbphs.tavs = WBPHSHistoric %>%
    dplyr::filter(Species == "CCGO", Year %in% tavs.year, Stratum %in% c(99, 10, 11)) %>%
    dplyr::select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    dplyr::rename(itotal=itotal.est, ibb=ibb.est) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))



  tavs = bind_rows(acp.tavs, ykg.tavs, wbphs.tavs) %>%
    mutate(Species="TAVS")


  # PACIFIC WHITEFRONTS
  #
  #Estimates and se’s for 1) indicated total birds, 2) indicated singles and paired
  #
  # 1)	YKG: all strata combined
  # 2)	WBPHS:
  #  a)	stratum 8
  #  b)	clipped stratum 99
  # 3)	Total and se for all surveys combined

  ykg.pw = YKGHistoric$combined %>%
    dplyr::filter(Species == "GWFG", Year %in% pw.year) %>%
    dplyr::select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to = "type", values_to = "estimate") %>%
    dplyr::mutate(area="YKG", Stratum="all")


  wbphs.pw = WBPHSHistoric %>%
    dplyr::filter(Species == "GWFG", Year %in% pw.year, Stratum %in% c(99, 8)) %>%
    dplyr::select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    dplyr::rename(itotal=itotal.est, ibb=ibb.est) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))

  pgwfg = bind_rows(ykg.pw, wbphs.pw) %>%
    mutate(Species="PGWFG")


  #WESTERN TUNDRA SWANS
  #
  #Estimates and se’s for 1) total birds, 2) singles and paired (note these are NOT indicated)
  #
  #1)	YKG: all strata combined
  #2)	WBPHS:
  #  a)	stratum 8
  #  b)	clipped stratum 99
  #  c)	stratum 10
  #  d)	stratum 11
  #3)	Total and se for all surveys combined

  ykg.tusw = YKGHistoric$combined %>%
    dplyr::filter(Species == "SWAN", Year %in% tusw.year) %>%
    dplyr::select(Year, total, total.se, sing1pair2, sing1pair2.se) %>%
    tidyr::pivot_longer(cols=total:sing1pair2.se, names_to = "type", values_to = "estimate") %>%
    dplyr::mutate(area="YKG", Stratum="all")

  wbphs.tusw = WBPHSHistoric %>%
    dplyr::filter(Species == "SWAN", Year %in% tusw.year, Stratum %in% c(8, 99, 10, 11)) %>%
    dplyr::select(Year, Stratum, total.est, total.se, sing1pair2.est, sing1pair2.se) %>%
    dplyr::rename(total=total.est, sing1pair2=sing1pair2.est) %>%
    tidyr::pivot_longer(cols=total:sing1pair2.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))


  wtusw = bind_rows(ykg.tusw, wbphs.tusw) %>%
    mutate(Species="WTUSW")

  # LESSERS
  #
  #Estimates and se’s for 1) indicated total birds, 2) indicated singles and paired
  #
  #1)	WBPHS:
  #a)	stratum 1
  #b)	stratum 2
  #c)	stratum 3
  #d)	stratum 4
  #e)	stratum 12
  #2)	Total and se for all of the above strata combined

  wbphs.lesser = WBPHSHistoric %>%
    dplyr::filter(Species == "CCGO", Year %in% lesser.year, Stratum %in% c(1, 2, 3, 4, 12)) %>%
    dplyr::select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    dplyr::rename(itotal=itotal.est, ibb=ibb.est) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))

  lesser = wbphs.lesser %>%
    mutate(Species="LESSER")


  #MID CONTINENT WHITEFRONTS
  #
  #  Estimates and se’s for 1) indicated total birds, 2) indicated singles and paired
  #
  #1)	WBPHS:
  # a)	stratum 3
  # b)	stratum 4
  # c)	stratum 5
  # d)	stratum 6
  # e)	stratum 10
  # f)	stratum 11
  #2)	Total and se for all of the above WBPHS strata combined
  #3)	ACP


  acp.mcw = ACPHistoric$combined %>%
    dplyr::filter(Species == "GWFG", Year %in% mcw.year) %>%
    dplyr::select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to = "type", values_to = "estimate") %>%
    dplyr::mutate(area="ACP", Stratum="all")


  wbphs.mcw = WBPHSHistoric %>%
    dplyr::filter(Species == "GWFG", Year %in% mcw.year, Stratum %in% c(3, 4, 5, 6, 10, 11)) %>%
    dplyr::select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    dplyr::rename(itotal=itotal.est, ibb=ibb.est) %>%
    tidyr::pivot_longer(cols=itotal:ibb.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))

  mcgwfg = bind_rows(acp.mcw, wbphs.mcw) %>%
    mutate(Species="MCGWFG")



  #PACIFIC COAST TRUMPETER SWAN
  #
  #Estimates and se’s for 1) total birds, 2) singles and paired (note these are NOT indicated)
  #
  #1)	WBPHS:
  #a)	stratum 1
  #b)	stratum 2
  #c)	stratum 3
  #d)	stratum 4
  #e)	stratum 6
  #f)	stratum 7
  #2)	Total and se for all of the above strata combined

  wbphs.trsw = WBPHSHistoric %>%
    dplyr::filter(Species == "SWAN", Year %in% trsw.year, Stratum %in% c(1, 2, 3, 4, 6, 7)) %>%
    dplyr::select(Year, Stratum, total.est, total.se, sing1pair2.est, sing1pair2.se) %>%
    dplyr::rename(total=total.est, sing1pair2=sing1pair2.est) %>%
    tidyr::pivot_longer(cols=total:sing1pair2.se, names_to="type", values_to="estimate") %>%
    dplyr::mutate(area="WBPHS", Stratum=as.character(Stratum))


  pctrsw = wbphs.trsw %>%
    mutate(Species="PCTRSW")


  composite = bind_rows(tavs, pgwfg, wtusw, lesser, mcgwfg, pctrsw)

  return(composite)

  }

