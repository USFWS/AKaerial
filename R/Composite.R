

CompositeIndices = function(tavs.year = c(1985:2021),
                            pw.year = c(1985:2021),
                            tusw.year = c(1985:2021),
                            lesser.year = c(1964:2021),
                            mcw.year = c(1964:2021),
                            trsw.year = c(1964:2021),
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
    filter(Species == "CCGO", Year %in% tavs.year) %>%
    select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    rename(itotal_ACP=itotal, itotal.se_ACP = itotal.se, ibb_ACP=ibb, ibb.se_ACP=ibb.se)

  ykg.tavs = YKGHistoric$combined %>%
    filter(Species == "TAVS", Year %in% tavs.year) %>%
    select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    rename(itotal_YKG=itotal, itotal.se_YKG = itotal.se, ibb_YKG=ibb, ibb.se_YKG=ibb.se)

  wbphs.tavs = WBPHSHistoric %>%
    filter(Species == "CCGO", Year %in% tavs.year, Stratum %in% c(99, 10, 11)) %>%
    select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    rename(itotal=itotal.est, ibb=ibb.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(itotal, itotal.se, ibb, ibb.se)) %>%
    select(Year, itotal_99, itotal.se_99, itotal_10, itotal.se_10, itotal_11, itotal.se_11,
           ibb_99, ibb.se_99, ibb_10, ibb.se_10, ibb_11, ibb.se_11, )

  tavs.table = data.frame("Year"=tavs.year) %>%
    merge(acp.tavs, by="Year", all.x=TRUE) %>%
    merge(ykg.tavs, by="Year", all.x=TRUE) %>%
    merge(wbphs.tavs, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(itotal.all = sum(itotal_ACP, itotal_YKG, itotal_10, itotal_11, itotal_99),
           itotal.se = sqrt(sum(itotal.se_ACP^2, itotal.se_YKG^2, itotal.se_10^2, itotal.se_11^2, itotal.se_99^2)),
           ibb.all = sum(ibb_ACP, ibb_YKG, ibb_10, ibb_11, ibb_99),
           ibb.se = sqrt(sum(ibb.se_ACP^2, ibb.se_YKG^2, ibb.se_10^2, ibb.se_11^2, ibb.se_99^2))) %>%
    relocate(starts_with("ibb"), .after=starts_with("itotal"))


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
    filter(Species == "GWFG", Year %in% pw.year) %>%
    select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    rename(itotal_YKG=itotal, itotal.se_YKG = itotal.se, ibb_YKG=ibb, ibb.se_YKG=ibb.se)

  wbphs.pw = WBPHSHistoric %>%
    filter(Species == "GWFG", Year %in% pw.year, Stratum %in% c(99, 8)) %>%
    select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    rename(itotal=itotal.est, ibb=ibb.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(itotal, itotal.se, ibb, ibb.se)) %>%
    select(Year, itotal_8, itotal.se_8, itotal_99, itotal.se_99,
           ibb_8, ibb.se_8, ibb_99, ibb.se_99 )

  pw.table = data.frame("Year"=pw.year) %>%
    merge(ykg.pw, by="Year", all.x=TRUE) %>%
    merge(wbphs.pw, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(itotal.all = sum(itotal_YKG, itotal_8, itotal_99),
           itotal.se = sqrt(sum(itotal.se_YKG^2, itotal.se_8^2, itotal.se_99^2)),
           ibb.all = sum(ibb_YKG, ibb_8, ibb_99),
           ibb.se = sqrt(sum(ibb.se_YKG^2, ibb.se_8^2, ibb.se_99^2))) %>%
    relocate(starts_with("ibb"), .after=starts_with("itotal"))


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
    filter(Species == "TUSW", Year %in% tusw.year) %>%
    select(Year, total, total.se, sing1pair2, sing1pair2.se) %>%
    rename(total_YKG=total, total.se_YKG = total.se, sing1pair2_YKG=sing1pair2, sing1pair2.se_YKG=sing1pair2.se)

  wbphs.tusw = WBPHSHistoric %>%
    filter(Species == "SWAN", Year %in% tusw.year, Stratum %in% c(8, 99, 10, 11)) %>%
    select(Year, Stratum, total.est, total.se, sing1pair2.est, sing1pair2.se) %>%
    rename(total=total.est, sing1pair2=sing1pair2.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(total, total.se, sing1pair2, sing1pair2.se)) %>%
    select(Year, total_8, total.se_8, total_99, total.se_99, total_10, total.se_10, total_11, total.se_11,
           sing1pair2_8, sing1pair2.se_8, sing1pair2_99, sing1pair2.se_99, sing1pair2_10, sing1pair2.se_10, sing1pair2_11, sing1pair2.se_11)

  tusw.table = data.frame("Year"=tusw.year) %>%
    merge(ykg.tusw, by="Year", all.x=TRUE) %>%
    merge(wbphs.tusw, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(total.all = sum(total_YKG, total_8, total_99, total_10, total_11),
           total.se = sqrt(sum(total.se_YKG^2, total.se_8^2, total.se_99^2, total.se_10^2, total.se_11^2)),
           sing1pair2.all = sum(sing1pair2_YKG, sing1pair2_8, sing1pair2_99, sing1pair2_10, sing1pair2_11),
           sing1pair2.se = sqrt(sum(sing1pair2.se_YKG^2, sing1pair2.se_8^2, sing1pair2.se_99^2, sing1pair2.se_10^2, sing1pair2.se_11^2))) %>%
    relocate(starts_with("sing1pair2"), .after=starts_with("total"))

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
    filter(Species == "CCGO", Year %in% lesser.year, Stratum %in% c(1, 2, 3, 4, 12)) %>%
    select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    rename(itotal=itotal.est, ibb=ibb.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(itotal, itotal.se, ibb, ibb.se)) %>%
    select(Year, itotal_1, itotal.se_1, itotal_2, itotal.se_2, itotal_3, itotal.se_3, itotal_4, itotal.se_4, itotal_12, itotal.se_12,
           ibb_1, ibb.se_1, ibb_2, ibb.se_2, ibb_3, ibb.se_3, ibb_4, ibb.se_4, ibb_12, ibb.se_12 )

  lesser.table = data.frame("Year"=lesser.year) %>%
    merge(wbphs.lesser, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(itotal.all = sum(itotal_1, itotal_2, itotal_3, itotal_4, itotal_12),
           itotal.se = sqrt(sum(itotal.se_1^2, itotal.se_2^2, itotal.se_3^2, itotal.se_4^2, itotal.se_12^2)),
           ibb.all = sum(ibb_1, ibb_2, ibb_3, ibb_4, ibb_12),
           ibb.se = sqrt(sum(ibb.se_1^2, ibb.se_2^2, ibb.se_3^2, ibb.se_4^2, ibb.se_12^2))) %>%
    relocate(starts_with("ibb"), .after=starts_with("itotal"))


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
    filter(Species == "GWFG", Year %in% mcw.year) %>%
    select(Year, itotal, itotal.se, ibb, ibb.se) %>%
    rename(itotal_ACP=itotal, itotal.se_ACP = itotal.se, ibb_ACP=ibb, ibb.se_ACP=ibb.se)


  wbphs.mcw = WBPHSHistoric %>%
    filter(Species == "GWFG", Year %in% mcw.year, Stratum %in% c(3, 4, 5, 6, 10, 11)) %>%
    select(Year, Stratum, itotal.est, itotal.se, ibb.est, ibb.se) %>%
    rename(itotal=itotal.est, ibb=ibb.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(itotal, itotal.se, ibb, ibb.se)) %>%
    select(Year, itotal_3, itotal.se_3, itotal_4, itotal.se_4, itotal_5, itotal.se_5, itotal_6, itotal.se_6, itotal_10, itotal.se_10, itotal_11, itotal.se_11,
           ibb_3, ibb.se_3, ibb_4, ibb.se_4, ibb_5, ibb.se_5, ibb_6, ibb.se_6, ibb_10, ibb.se_10, ibb_11, ibb.se_11 )


  mcw.table = data.frame("Year"=mcw.year) %>%
    merge(acp.mcw, by="Year", all.x=TRUE) %>%
    merge(wbphs.mcw, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(itotal.wbphs = sum(itotal_3, itotal_4, itotal_5, itotal_6, itotal_10, itotal_11),
           itotal.wbphs.se = sqrt(sum(itotal.se_3^2, itotal.se_4^2, itotal.se_5^2, itotal.se_6^2, itotal.se_10^2, itotal.se_11^2)),
           ibb.wbphs = sum(ibb_3, ibb_4, ibb_5, ibb_6, ibb_10, ibb_11),
           ibb.wbphs.se = sqrt(sum(ibb.se_3^2, ibb.se_4^2, ibb.se_5^2, ibb.se_6^2, ibb.se_10^2, ibb.se_11^2))) %>%
    relocate(starts_with("ibb"), .after=starts_with("itotal"))



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
    filter(Species == "SWAN", Year %in% trsw.year, Stratum %in% c(1, 2, 3, 4, 6, 7)) %>%
    select(Year, Stratum, total.est, total.se, sing1pair2.est, sing1pair2.se) %>%
    rename(total=total.est, sing1pair2=sing1pair2.est) %>%
    pivot_wider(names_from=Stratum, values_from = c(total, total.se, sing1pair2, sing1pair2.se)) %>%
    select(Year, total_1, total.se_1, total_2, total.se_2, total_3, total.se_3, total_4, total.se_4, total_6, total.se_6, total_7, total.se_7,
           sing1pair2_1, sing1pair2.se_1, sing1pair2_2, sing1pair2.se_2, sing1pair2_3, sing1pair2.se_3, sing1pair2_4, sing1pair2.se_4, sing1pair2_6, sing1pair2.se_6, sing1pair2_7, sing1pair2.se_7)

  trsw.table = data.frame("Year"=trsw.year) %>%
    merge(wbphs.trsw, by="Year", all.x=TRUE) %>%
    group_by(Year) %>%
    mutate(total.all = sum(total_1, total_2, total_3, total_4, total_6, total_7),
           total.se = sqrt(sum(total.se_1^2, total.se_2^2, total.se_3^2, total.se_4^2, total.se_6^2, total.se_7^2)),
           sing1pair2.all = sum(sing1pair2_1, sing1pair2_2, sing1pair2_3, sing1pair2_4, sing1pair2_6, sing1pair2_7),
           sing1pair2.se = sqrt(sum(sing1pair2.se_1^2, sing1pair2.se_2^2, sing1pair2.se_3^2, sing1pair2.se_4^2, sing1pair2.se_6^2, sing1pair2.se_7^2))) %>%
    relocate(starts_with("sing1pair2"), .after=starts_with("total"))


  write.csv(tavs.table, "CompositeTAVS.csv", row.names = FALSE, quote=FALSE)
  write.csv(pw.table, "CompositePGWFG.csv", row.names = FALSE, quote=FALSE)
  write.csv(tusw.table, "CompositeTUSW.csv", row.names = FALSE, quote=FALSE)
  write.csv(lesser.table, "CompositeLECG.csv", row.names = FALSE, quote=FALSE)
  write.csv(mcw.table, "CompositeMCGWFG.csv", row.names = FALSE, quote=FALSE)
  write.csv(trsw.table, "CompositeTRSW.csv", row.names = FALSE, quote=FALSE)


  if(versioning==TRUE){
    acp.version = file.info("./data/ACPHistoric.rda")$mtime
    ykg.version = file.info("./data/YKGHistoric.rda")$mtime
    wbphs.version = file.info("./data/WBPHSHistoric.rda")$mtime

    versions = data.frame("Survey"=c("ACP", "WBPHS", "YKG"), "Version"=c(acp.version, wbphs.version, ykg.version))

    write.csv(versions, "Versioning.csv", row.names=FALSE, quote=FALSE)

  }


}
