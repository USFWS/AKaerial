#' YKDVEst uses AKaerial tables to generate a VCF-corrected estimate of SPEI on the YKD
#' @return A list containing population estimates of 'indicated breeding birds', the 'index" estimates (without detection correction), standard errors of each, and a ggplot objects of each by year.
YKDVEst <- function(){
  #Code to calculate detection-corrected population estimates from stratified plot data
  # using a ratio estimator. The approach is from Fieberg and Giudice (2008, JWM 72:837),
  # hereafter F&G.
  #The key parts are equations A3 and A4 of F&G.  A3 is explained in F&G;
  # A4 is explained best in the text book of Thompson (2012, Sampling).
  #Key difference from above citation is the use of the ratio estimator
  #for the mean number observed across transects.  This was not in F&G or Thompson.

  #Code originally written by Erik Osnas, December 2018. Modified in January & February 2019.
  #Modified 20190925 for data request from Kylee Durham.
  # Major update in August, 2026 by C Frost
  # write as function that accepts dataframe of transect level summaries, and output VCF corrected population estimates.
  #Copyright:  GNU General Public License v3.0


  dat <- MasterSummary %>%
    filter(Survey=="YKDV") %>%
    mutate(vcf_stratum = sub(".*-", "", strata))


  vcfData <- data.frame(vcf_stratum = c("High", "Low", "Medium"),
                        vcf = c(3.09395, 1.354825, 2.459412),
                        vcfse = c(0.1873677, 0.1502611, 0.1757193))

  dat <- dat[dat$vcf_stratum %in% vcfData$vcf_stratum,]

  #Need total number of possible transects for each strata. These vary by year.
  #Based on Current (as of 20260806) YKD 'tyler density' strata (original used for paper is different)
  #from AKaerial

  Mdat <- MasterStrata %>%
    filter(Survey=="YKDV") %>%
    mutate(vcf_stratum = sub(".*-", "", Stratum))

  Mdat <- Mdat[Mdat$vcf_stratum %in% vcfData$vcf_stratum,]
  Mdat$M <- round(Mdat$M)



dat_summary <- dat %>%
  group_by(Year, Observer, Species, ctran, area, strata,
           Seat, Survey, vcf_stratum) %>%   # list every column that's constant per ctran
  summarise(
    Pairs   = sum(Num[Obs_Type == "pair"], na.rm = TRUE),
    Singles = sum(Num[Obs_Type %in% c("single", "flkdrake4")], na.rm = TRUE),
    .groups = "drop"
  )

dat=dat_summary


dat <- left_join(dat, vcfData[,1:3])

colnames(dat)[colnames(dat)=="se"]="vcfse"

########################
#find mean by statum and year
msing <- aggregate(dat$Singles, by=list(strata=dat$strata, Year=dat$Year), mean)

mpair <- aggregate(dat$Pairs, by=list(strata=dat$strata, Year=dat$Year), mean)

mx <- aggregate(dat$area, by=list(strata=dat$strata, Year=dat$Year), mean) #, na.rm=TRUE

Area <- aggregate(Mdat$layer.area, by=list(strata=Mdat$Stratum, Year=Mdat$Year), mean)

colnames(msing)[3]="msing"
colnames(mpair)[3]="mpair"
colnames(mx)[3]="marea"
colnames(Area)[3]="sarea"

merged=left_join(msing, mpair)
merged=left_join(merged, mx)
merged=left_join(merged, Area)

merged = merged %>% mutate(Ysing=sarea*msing/marea) %>% mutate(Ypair=sarea*mpair/marea) %>% mutate(Yibb=2*(Ysing+Ypair))

merged = left_join(merged, dat %>% select(Year, strata, vcf_stratum, vcf, vcfse)) %>% distinct()

merged = left_join(merged, Mdat %>% select(Year, strata=Stratum, M)) %>% distinct()

merged = merged %>% mutate(Nsing=vcf * Ysing, Npair=vcf*Ypair) %>% mutate(Nibb = vcf*Yibb)

m = dat %>% group_by(Year, strata) %>% summarise(m=n())
merged = merged %>% left_join(m)


###############################################################################
#Find variance of estimates


vars = dat %>% group_by(Year, strata) %>% summarise(sVar=var(Singles), pVar=var(Pairs), sCov=cov(Singles, area), pCov=cov(Pairs, area), aVar=var(area))

merged = merged %>% left_join(vars)
library(dplyr)
library(tidyr)



## -----------------------------------------------------------------------
## STEP 1: Stratum-level sampling variance (per-transect / density scale)
##   Ratio-estimator variance for the mean density, singles and pairs
##   separately. This is the "VarIndexSing" / "VarIndexPair" piece from
##   the F&G code -- i.e. sampling variance only, no VCF/binomial term yet.
## -----------------------------------------------------------------------

merged <- merged %>%
  mutate(
    varS_sample = (1 - m/M) * (sVar + (msing/marea)^2*aVar - 2*(msing/marea)*sCov) / m,
    varP_sample = (1 - m/M) * (pVar + (mpair/marea)^2*aVar - 2*(mpair/marea)*pCov) / m
  )

## -----------------------------------------------------------------------
## STEP 2: Add the binomial detection-variance component
##   Accounts for uncertainty from the detection process itself (each
##   animal independently detected with probability ~1/vcf), which is
##   separate from ordinary sampling variance.
##   Formula (F&G eqn A4): (sarea/M) * (my/marea) * (vcf-1) / (vcf*M)
## -----------------------------------------------------------------------

merged <- merged %>%
  mutate(
    varS_full = varS_sample + (sarea/M) * (msing/marea) * (vcf - 1) / (vcf * M),
    varP_full = varP_sample + (sarea/M) * (mpair/marea) * (vcf - 1) / (vcf * M)
  )

## -----------------------------------------------------------------------
## STEP 3: Reshape to long format (stack singles and pairs)
##   Singles and pairs share the same VCF within a stratum, so they need
##   to be combined in the same VCF-uncertainty calculation (step 4).
## -----------------------------------------------------------------------

to_long <- function(df, y_col, var_col, vcfse_col = "vcfse") {
  df %>%
    transmute(
      Year, strata, vcf_stratum, m, M, sarea, marea,
      my       = .data[[y_col]],
      VarYHat  = .data[[var_col]],
      vcf,
      vcfse    = .data[[vcfse_col]]
    )
}

merged_long <- bind_rows(
  to_long(merged, "msing", "varS_full"),
  to_long(merged, "mpair", "varP_full")
)

## -----------------------------------------------------------------------
## STEP 3b: Pool variance across strata sharing a vcf_stratum, within the
##   same year, for any stratum where VarYHat is NA (typically m=1, so
##   sample variance is undefined). Borrows from sibling strata in the
##   same Year x vcf_stratum group rather than discarding the stratum's
##   point estimate contribution entirely.
## -----------------------------------------------------------------------

merged_long <- merged_long %>%
  group_by(Year, vcf_stratum) %>%
  mutate(
    n_available    = sum(!is.na(VarYHat)),
    VarYHat_pooled = ifelse(
      is.na(VarYHat),
      mean(VarYHat[!is.na(VarYHat)]),
      VarYHat
    )
  ) %>%
  ungroup()

## Flag any Year x vcf_stratum groups with NOTHING to pool from --
## these need a separate fallback (e.g. borrow across years instead).
no_pool_available <- merged_long %>%
  filter(is.na(VarYHat), n_available == 0) %>%
  distinct(Year, strata, vcf_stratum)

if (nrow(no_pool_available) > 0) {
  warning("Some stratum-years have no sibling data to pool variance from. ",
          "See `no_pool_available` for details.")
}

## -----------------------------------------------------------------------
## STEP 4: F&G equation A3 -- combine variance across strata that SHARE
##   a common VCF estimate. Strata sharing a VCF have correlated VCF
##   error, so their density-ratio means and variances must be summed
##   BEFORE the VCF variance formula is applied -- not stratum by stratum.
##   Uses VarYHat_pooled so m=1 stratum-years still contribute a variance
##   estimate borrowed from sibling strata in the same VCF group/year.
## -----------------------------------------------------------------------

varFG <- function(data, vcf_val, vcf_se) {
  data %>%
    group_by(Year) %>%
    summarise(
      SumAy   = sum(sarea * (my / marea)),        # combined ratio-estimate total, across shared-VCF strata
      SumA2vy = sum(VarYHat_pooled * M^2),         # combined density-variance scaled to totals
      .groups = "drop"
    ) %>%
    mutate(
      Var = SumAy^2 * vcf_se^2 + vcf_val^2 * SumA2vy - vcf_se^2 * SumA2vy
    ) %>%
    select(Year, Var)
}

## -----------------------------------------------------------------------
## STEP 5: Apply varFG() once per vcf_stratum group (the level at which
##   VCF is actually estimated), NOT the finer restratification `strata`.
##   vcfData should have one row per vcf_stratum, with columns:
##   vcf_stratum, vcf, vcfse
## -----------------------------------------------------------------------

vcf_strata <- unique(merged_long$vcf_stratum)

var_by_group <- lapply(vcf_strata, function(grp) {

  vcf_row <- vcfData %>% filter(vcf_stratum == grp)

  if (nrow(vcf_row) != 1) {
    stop(sprintf("Expected exactly 1 row in vcfData for vcf_stratum = '%s', found %d",
                 grp, nrow(vcf_row)))
  }

  varFG(
    data    = merged_long %>% filter(vcf_stratum == grp),
    vcf_val = vcf_row$vcf,
    vcf_se  = vcf_row$vcfse    # <- update this name to match names(vcfData) if different
  ) %>%
    rename(!!paste0("Var_", grp) := Var)
})

## -----------------------------------------------------------------------
## STEP 6: Sum variance across all VCF groups, scale by 4 (from ibb = 2*(S+P))
## -----------------------------------------------------------------------

VarData <- Reduce(function(x, y) left_join(x, y, by = "Year"), var_by_group)

VarData <- VarData %>%
  mutate(VarTotal = 4 * rowSums(select(., -Year), na.rm = TRUE))

## -----------------------------------------------------------------------
## STEP 7: Uncorrected index (no VCF) totals, for comparison to the
##   VCF-corrected estimate. Strata are independent here (no shared-VCF
##   correlation to account for), so this is a plain sum of Yibb and its
##   variance across ALL strata within a year.
##   varS_full/varP_full are on the DENSITY scale (variance of msing/marea),
##   matching how varS/varP were used earlier -- so they need the same
##   sarea^2 scaling to reach the stratum-TOTAL scale before summing.
## -----------------------------------------------------------------------

merged <- merged %>%
  mutate(
    varYsing = sarea^2 * varS_full,
    varYpair = sarea^2 * varP_full,
    varYibb  = 4 * (varYsing + varYpair)
  )

index_totals <- merged %>%
  group_by(Year) %>%
  summarise(
    Yibb_total    = sum(Yibb, na.rm = TRUE),
    VarYibb_total = sum(varYibb, na.rm = TRUE),
    .groups = "drop"
  )

## -----------------------------------------------------------------------
## STEP 8: Combine corrected (Nibb/VarTotal) and uncorrected (Yibb/VarIndex)
##   into one summary table.
## -----------------------------------------------------------------------

year_totals <- merged %>%
  group_by(Year) %>%
  summarise(Nibb_total = sum(Nibb, na.rm = TRUE), .groups = "drop") %>%
  left_join(VarData %>% select(Year, VarTotal), by = "Year") %>%
  left_join(index_totals, by = "Year") %>%
  rename(VarIndex = VarYibb_total, Yibb = Yibb_total) %>%
  mutate(
    seNibb   = sqrt(VarTotal),
    seIndex  = sqrt(VarIndex),
    cvNibb   = seNibb / Nibb_total,
    cvIndex  = seIndex / Yibb,
    C_Nibb   = exp(1.96 * sqrt(log(1 + cvNibb^2))),
    C_Index  = exp(1.96 * sqrt(log(1 + cvIndex^2))),
    lclNibb  = Nibb_total / C_Nibb,
    uclNibb  = Nibb_total * C_Nibb,
    lclIndex = Yibb / C_Index,
    uclIndex = Yibb * C_Index
  )

popest <- year_totals %>%
  select(Year, Nibb = Nibb_total, seNibb, cvNibb, lclNibb, uclNibb,
         Yibb, seIndex, cvIndex, lclIndex, uclIndex)

## -----------------------------------------------------------------------
## STEP 9: Plot -- corrected (Nibb) vs uncorrected (Yibb) index over time
## -----------------------------------------------------------------------

library(ggplot2)

jig <- 0.1

plotData1 <- popest %>%
  transmute(
    Year  = Year + jig,
    type  = "Density-adjusted VCF",
    Ibb   = Nibb,
    lower = pmax(Nibb - 2*seNibb, 0),
    upper = Nibb + 2*seNibb
  )

plotData2 <- popest %>%
  transmute(
    Year  = Year - jig,
    type  = "Index",
    Ibb   = Yibb,
    lower = pmax(Yibb - 2*seIndex, 0),
    upper = Yibb + 2*seIndex
  )

plotData <- bind_rows(plotData1, plotData2)

year_min <- min(popest$Year, na.rm = TRUE)
year_max <- max(popest$Year, na.rm = TRUE)

gplot <- ggplot() +
  geom_point(data = plotData, aes(x = Year, y = Ibb, color = type), size = 2) +
  labs(title = "", x = "Year", y = "Indicated Breeding Birds") +
  scale_x_continuous(breaks = seq(year_min, year_max, by = 2), limits = c(year_min-.5, year_max+.5)) +
  geom_linerange(data = plotData, aes(x = Year, ymin = lower, ymax = upper, color = type), linewidth = 1.1) +
  scale_colour_manual(values = c("black", "gray50")) +
  theme(legend.position = c(0.2, 0.75), legend.title = element_blank())

gplot

## -----------------------------------------------------------------------
## STEP 10: Final return object
## -----------------------------------------------------------------------

result <- list(popest = popest, plot = gplot)
result
}

