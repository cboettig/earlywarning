#' @title stability model fitting
#'
#' @param X data sampling time and sampled value as columns. If only
#' one column is provided, assumes that the time is uniform
#' @param model the model used
#' @param p initial parameter guesses, if NULL, will guess from data
#' @param ... additional parameters passed to the optim routine
#' @param store_data if FALSE will not return an object with 
#' a copy of the original data
#' @return a gauss model fit object (list), containing
#' the data (if requested), the model type used, the maximum-likelihood
#' estimated parameters, the log-likelihood at those values, 
#' and a convergence indicator
#' @details depends on the gauss class of functions and the model 
#' definitions LSN and constOU.  
#' @import odesolve
#' @export
stability_model <- function(X, model=c("LSN", "OU"), p = NULL, ..., 
                            store_data=TRUE){

  # reformat time series objects into proper data frames
  if(is(X, "ts"))
    X <- data.frame(as.numeric(time(X)), X@.Data)
  # if time values are not provided
  else if(is.null(dim(X)))
    X <- data.frame(1:length(X), X)
  
  # Estimate reasonable starting parameters for model specified #
  model <- match.arg(model)
  if(model=="LSN"){
    setmodel <- LSN 
  if(is.null(p)){
      p <- c(1/max(time(X[,1])), mean(X[,2]),sd(X[,2]))
      Ro <- p[1]^2
      theta <- p[2]+p[1]
      sigma<- abs(p[3]/sqrt(2*p[1]+ p[2]))
      p <- c(Ro=Ro, m=0, theta=theta, sigma=sigma)
    }
    } else if(model=="OU"){
    setmodel <- constOU
    if(is.null(p)){
      p <- c(Ro=1/max(time(X[,1])), theta=mean(X[,2]),
        sigma=sd(X[,2]))
    }
  } else {
    stop(paste("Model", model, "not recognized"))
  }
  #---------------------------------------------------------#

  ## Define the likelihood function and compute maximum likelihood
  f <- function(p){
      n <- length(X[,1])
      out <- -sum(dc.gauss(setmodel, X[2:n,2], X[1:(n-1),2], to=X[1:(n-1),1],
                            t1=X[2:n,1], p, log=T))
      out
  }
  o <- optim(p, f, ...)

  if(!store_data) # remove the data object to save space?
    X <- NULL
  # format the output
  out <- list(X=X, pars=o$par, model=model, loglik = o$value,
       convergence=(o$convergence==0) )
  class(out) <- c("gauss", "list")
  out
}


