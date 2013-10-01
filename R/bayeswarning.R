library(earlywarning)
data(chemostat)
X <- data.frame(as.numeric(time(chemostat)), chemostat@.Data)
logpriors <-  function(pars) sum(log(sapply(pars, dunif, min = 1e-20, max = 100)))
#out <- lsn_mcmc(X, logpriors, inits=NULL, n = 1e4)
  
#' mcmc posteriors for time-dependent OU model
#' 
#' @return the MCMC output, as constructed by \link{metrop} from the mcmc package.
#' @export
#' @import mcmc  
lsn_mcmc <- function(X, logpriors, inits=NULL, n = 1e4){

  if(is.null(inits)){
    p_est <- c(Ro=1/max(time(X[,1])), theta=mean(X[,2]), sigma=sd(X[,2])) # OU-version
    # Corrections for LSN model
    Ro <- as.numeric(p_est[1]^2)
    theta <- as.numeric(p_est[2]+p_est[1])
    sigma <- as.numeric(abs(p_est[3]/sqrt(2*p_est[1]+ p_est[2])))
    inits <- c(Ro=Ro, m=1e-20, theta=theta, sigma=sigma)
  }
  
  posterior <- function(pars, X){ 
    n <- length(X[,1])
    mloglik <- -sum(dc.gauss(LSN, 
                         X[2:n,2], 
                         X[1:(n-1),2], 
                         to = X[1:(n-1),1],
                         t1 = X[2:n,1], 
                         pars, 
                         log=T))
    if(abs(mloglik) == Inf | is.nan(mloglik))
      mloglik <- 1e19
    
    -mloglik + logpriors(pars)
  }
  
  out <- metrop(obj = posterior, initial = inits, nbatch = n, X = X)  
  
  # assemble output
  out$priors <- priors
  out$obs <- X
  out
}





summary_lsn_mcmc <- function(gp, burnin=0, thin=1){
  
  postdist <- cbind(index=1:gp$nbatch, as.data.frame(exp(gp$batch)))
  s <- seq(burnin+1, gp$nbatch, by=thin)
  postdist <- postdist[s,]
  names(postdist) <- c("index", names(gp$initial))
  
  # TRACES
  df <- melt(postdist, id="index")
  traces_plot <- 
    ggplot(df) + geom_line(aes(index, value)) + 
    facet_wrap(~ variable, scale="free", ncol=1)
  
  
  prior_curves <- ddply(df, "variable", function(dd){
    grid <- seq(min(dd$value), max(dd$value), length = 100)
    data.frame(value = grid, density = prior_fns[[dd$variable[1]]](grid))
  })
  
  # Posteriors (easier to read than histograms)
  posteriors_plot <- ggplot(df, aes(value)) + 
    stat_density(geom="path", position="identity", alpha=0.7) +
    geom_line(data=prior_curves, aes(x=value, y=density), col="red") + 
    facet_wrap(~ variable, scale="free", ncol=2)
  
  # print(traces_plot)
  #  print(posteriors_plot)
  
  out <- list(traces_plot = traces_plot, posteriors_plot = posteriors_plot)
  invisible(out)
}

