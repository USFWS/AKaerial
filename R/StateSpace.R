

#' Runs and reports on a generic state space model of population growth
#'
#' StateSpace will search the internal AKaerial index estimates and run a generic state space model over the range selected
#'
#' A state-space model is a general term that usually refers to a model containing the true underlying (and unobserved)
#'  time-dependent state of the system and imperfect time-dependent observations.
#'  The states change with time so that the state at time t depends on the state at time t-1
#'  (and potentially other factors).  Here, the state is defined as the unobserved total population of geese or ducks in the survey area, and observations depend just on the state.
#'  A formal description is contained in GenericStateSpaceModel.Rmd, which is provided as the output to this function
#'
#' @author \itemize{
#' \item{Charles Frost, \email{charles_frost@@fws.gov}}
#' \item{Erik Osnas, \email{erik_osnas@@fws.gov}}}
#'
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param area The area code for dedicated MBM Alaska region surveys.  Either folder.path or area must be specified.
#'    Acceptable values include:
#'  \itemize{
#'  \item YKD - Yukon Kuskokwim Delta, ducks
#'  \item YKG - Yukon Kuskokwim Delta, geese
#'  \item ACP - Arctic Coastal Plain
#'  \item CRD - Copper River Delta
#'  \item WBPHS - The North American Waterfowl Breeding Population Habitat Survey
#'  }
#' @param index The index used.
#'   Acceptable values include:
#'  \itemize{
#'  \item itotal - indicated total; singles and pairs doubled, flocks added, flkdrake added, flkdrake 2-4 doubled
#'  \item ibb - indicated breeding birds; singles and pairs doubled
#'  \item total - total birds observed; pairs doubled, others added}
#' @param years - The years to run the model over
#' @param species - The accepted \code{sppntable} species code for which to model
#' @param N1 - Initial population size in the format c(min, max) to establish log-scale uniformly-distributed prior
#' @param r - Range of growth rate parameter in the format c(min, max) to establish normally-distributed prior
#' @param sigma - Range of the process standard deviation parameter in the format c(min, max) to establish uniformly-distributed prior
#' @param n.chains - Number of Monte Carlo chains
#' @param n.thin - Model thin rate
#' @param n.iter - Model iterations
#' @param n.burnin - Burn-in iteration length
#'
#' @return Will generate an .html report to the current working directory
#'
#' @export
StateSpace=function(area, index, years, species, N1=c(1000,100000), r=c(-.3,.3), sigma=c(0,.3),
                    n.chains = 3, n.thin = 1, n.iter = 5000, n.burnin = 1000){

if(area=="ACP"){data=ACPHistoric$combined}
if(area=="CRD"){data=CRDHistoric$combined}
if(area=="YKD"){data=YKDHistoric$combined}
if(area=="YKG"){data=YKGHistoric$combined}

if(index=="itotal"){N=data$itotal[data$Year %in% years & data$Species %in% species]
                    SE=data$itotal.se[data$Year %in% years & data$Species %in% species]}
if(index=="total"){N=data$total[data$Year %in% years & data$Species %in% species]
                    SE=data$total.se[data$Year %in% years & data$Species %in% species]}
if(index=="ibb"){N=data$ibb[data$Year %in% years & data$Species %in% species]
                    SE=data$ibb.se[data$Year %in% years & data$Species %in% species]}

# plot(years, N, pch=16, ylim=c(0, max(N)+3*max(SE)), xlab="Year",
#          ylab="Population Estimate +/- 2SE",
#          main=paste("Observed Population- ", species, sep=""))
#     arrows(x0=years, y0=N-2*SE, y1=N+2*SE, length=0, col=1)

    cat("model{
       # Priors
       logN.est[1] ~ dunif(log(pN1min), log(pN1max))  # Prior for initial population size log scale
       mean.r ~ dunif(prmin, prmax)                   # Prior for mean growth rate
       sigma.proc ~ dunif(psigmamin, psigmamax)       # Prior for sd of state process log scale
       tau.proc <- pow(sigma.proc, -2)
       # Likelihood
       # State process
       for (t in 1:(T-1) ) {
         r[t] ~ dnorm(mean.r, tau.proc)
         logN.est[t+1] <- logN.est[t] + r[t]
       }
       # Observation process
       for (i in 1:T) {
         tau.obs[i] <- pow(sigma.obs[i], -2)
         y[i] ~ dnorm(exp(logN.est[i]), tau.obs[i])
       }
     }" , file = "ssm.jags", fill = TRUE)

    #structure data

    jags.data <- list(
      T = length(years),
      y = N,
      sigma.obs = SE,
      prmin=r[1],
      prmax=r[2],
      psigmamin=sigma[1],
      psigmamax=sigma[2],
      pN1min=N1[1],
      pN1max=N1[2]
      )

    print(jags.data)
    # Parameters monitored
    parameters <- c("logN.est", "mean.r", "sigma.proc")

    # Initial values
    inits <- function(){list(
      logN.est = c(runif(1, log(N1[1]+1), log(N1[2]-1)),rep(NA, length(N)-1)),
      mean.r = runif(1, -0.0001, 0.0001),
      sigma.proc = runif(1, 0.01, 0.011),
      r = c(runif(length(N)-1, -0.01, 0.01))
    )}

    out <- R2jags::jags(jags.data, inits, parameters, model.file="ssm.jags",
                           n.chains = n.chains, n.thin = n.thin, n.burnin = n.burnin,
                           n.iter = n.iter,
                           working.directory = getwd())


      # plot(out)
      #
      # hist(out$BUGSoutput$summary[,"Rhat"], col="lightgray",
      #      xlab="R-hat", main="R-hat should be near 1.0")
      #
      # R2jags::traceplot(out, ask=FALSE, varname ="mean.r")
      #
      # R2jags::traceplot(out, ask=FALSE, varname ="sigma.proc")

      sum.mean <- apply(exp(out$BUGSoutput$sims.list$logN.est), 2, mean)
      sum.sd <- apply(exp(out$BUGSoutput$sims.list$logN.est), 2, sd)
      sum.quant <- apply(exp(out$BUGSoutput$sims.list$logN.est), 2, quantile, probs = c(0.025, 0.5, 0.975))
      # plot(1,1, type="n", xlim=range(years), ylim=c(0, 1.5*max(N)), xlab="Year",
      #      ylab="Population Estimate",
      #      main="Observed Population and Bayesian Estimates")
      # polygon(x=c(years, rev(years)), y=c(sum.quant["2.5%",], rev(sum.quant["97.5%",])),
      #         col="lightgray")
      # arrows(x0=years, y0=N-2*SE, y1=N+2*SE, length=0, col=1)
      # points(years, N, pch=16)
      # lines(years, sum.mean, col=1, lwd=2)


      mean.r=out$BUGSoutput$sims.list$mean.r
      p.greater=length(mean.r[mean.r>0])/length(mean.r)

      td <- density(mean.r)
      maxDens <- which.max(td$y)

      r.plot= ggplot2::ggplot(as.data.frame(out$BUGSoutput$sims.list), ggplot2::aes(x=mean.r)) +
              ggplot2::geom_density(fill="gray") +
              ggplot2::geom_vline(xintercept = 0) +
              ggplot2::geom_label(ggplot2::aes(label=paste("P(r > 0.0) = ", (round(p.greater, 2))), x=0.05, y=0.1)) +
        ggplot2::ggtitle(expression(paste("Posterior of Mean Growth Rate"))) +
        ggplot2::geom_label(ggplot2::aes(label=paste("r = ", round(apply(out$BUGSoutput$sims.list$mean.r, 2, mean), 2),
                                   " (95% CI ", round(apply(out$BUGSoutput$sims.list$mean.r, 2, quantile, probs = c(0.025)),2),
                                   ", ",round(apply(out$BUGSoutput$sims.list$mean.r, 2, quantile, probs = c(0.975)),2), ")", sep=""), x=0, y=td$y[maxDens])) +

        ggplot2::xlab("Growth rate (r)") +
        ggplot2::ylab("Density") +
        ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold", hjust=0.5))



      # hist(out$BUGSoutput$sims.list$sigma.proc, xlim=c(0,0.3), xlab="", col="lightgray",
      #      main=expression(paste("Posterior of process standard deviation (",sigma["1"],")")))


      # kableExtra::kable(out$BUGSoutput$summary, "html", caption = "Parameter Summary") %>%
      #   kableExtra::kable_styling("striped", full_width = F)

        # save_kable("Summary.png")


        out.data=data.frame("Year"=years,
                            "N"=apply(exp(out$BUGSoutput$sims.list$logN.est), 2, mean),
                            "lower95"=apply(exp(out$BUGSoutput$sims.list$logN.est), 2, quantile, probs = c(0.025)),
                            "upper95"=apply(exp(out$BUGSoutput$sims.list$logN.est), 2, quantile, probs = c(0.975)),
                            "sd"=apply(exp(out$BUGSoutput$sims.list$logN.est), 2, sd),
                            "index"=N,
                            "l95index"=N-1.96*SE,
                            "u95index"=N+1.96*SE,
                            "indexSE"=SE)

       # knitr::kable(out.data, caption="State Space and Index Population Estimates",
       #                    col.names = c("Year", "N_S", "2.5 %", "97.5 %", "SD", "N(index)", "2.5 %", "97.5 %", "SE(index)" ), escape=FALSE) %>%
       #    kableExtra::kable_styling("striped", full_width = F)


         rmd.path=system.file("rmd/GenericStateSpaceModel.Rmd", package="AKaerial")

         rmarkdown::render(rmd.path, output_dir=getwd(), output_file=paste(species, "_SSM_", Sys.Date(), ".html", sep=''))






}
