#' Produce replicate-specific photo count estimates.
#'
#' Produce replicate-specific photo count estimates for transect-based photo count surveys.
#'
#' This function takes a data set of photos and associated automated or manual counts and
#' produces an estimate across transects at the replicate level.
#'
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#'
#' @param data An R data frame of photo counts and areas.  Needs a minimum structure with columns rep, transect, count, and area.
#' @param total.area The size of the study area to extrapolate density over.
#' @param M The maximum possible transects through the study area (used in variance calculation)
#'
#' @return A list of 2 objects: rep_summary for information regarding the variance, and est for the estimate by replicate.
#'
#' @export
PhotoEstimate = function(data, total.area, M){

  trans_summary = data %>%
    group_by(rep, transect) %>%
    summarize(count = sum(count),
              area = sum(area)
    )

  rep_summary = trans_summary %>%
    group_by(rep) %>%
    summarize(count.area.cov = cov(count, area),
              count.var = var(count),
              sampled.area = sum(area),
              sampled.area.var = var(area),
              m = length(unique(transect)),
              count = sum(count)
    ) %>%
    st_drop_geometry()


  est = rep_summary %>%
    group_by(rep) %>%
    summarize(
      dens = count/sampled.area,
      est = units::drop_units(dens * total.area),
      var = (M^2)*((1-(m/M))/m)*(count.var+(dens^2)*(sampled.area.var)-(2*dens*count.area.cov)),
      se = sqrt(var)
    ) %>%
    st_drop_geometry()

  return(list(rep_summary=rep_summary,
              est=est))

}

