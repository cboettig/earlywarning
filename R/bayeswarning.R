library(earlywarning)
data(chemostat)
X <- data.frame(as.numeric(time(chemostat)), chemostat@.Data)



logpriors <-  function(pars)
  unname(dlnorm(pars[1], log(inits[1]),  10, log=TRUE) + 
  dnorm(pars[2], inits[2],  100, log=TRUE) + 
  log(dunif(pars[3], 0,  100)) + 
  dlnorm(pars[4], log(inits[4]),  10, log=TRUE))
  
#logpriors <-  function(pars) sum(log(sapply(pars, dunif, min = 1e-20, max = 100)))
out <- lsn_mcmc(X, logpriors, inits=NULL, n = 1e4)
  
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





summary_lsn_mcmc <- function(obj, burnin=0, thin=1){
  
  postdist <- cbind(index=1:obj$nbatch, as.data.frame(exp(obj$batch)))
  s <- seq(burnin+1, obj$nbatch, by=thin)
  postdist <- postdist[s,]
  names(postdist) <- c("index", names(obj$initial))
  
  # TRACES
  df <- melt(postdist, id="index")
  traces_plot <- 
    ggplot(df) + geom_line(aes(index, value)) + 
    facet_wrap(~ variable, scale="free", ncol=1)
    
  # Posteriors (easier to read than histograms)
  posteriors_plot <- ggplot(df, aes(value)) + 
    stat_density(geom="path", position="identity", alpha=0.7) +
    facet_wrap(~ variable, scale="free", ncol=2) 
    
  out <- list(traces_plot = traces_plot, posteriors_plot = posteriors_plot)
  invisible(out)
}

