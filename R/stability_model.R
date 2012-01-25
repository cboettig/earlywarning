#' @param X data sampling time and sampled value as columns
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
#' @export
stability_model <- function(X, model=c("LSN", "OU"), p = NULL, ..., 
                            store_data=TRUE){

  # reformat time series objects into proper data frames
  if(is(X, "ts"))
    X <- data.frame(as.numeric(time(X)), X@.Data)

  # Select the model method and estimate starting parameters #
  model <- match.arg(model)
  if(model=="LSN"){
    setmodel <- LSN 
  if(is.null(p)){
      p <- c(Ro=1/max(time(X[,1])), theta=mean(X[,2]),
             sigma=sd(X[,2]))
      Ro <- p['Ro']^2
      theta <- p['theta']+p['Ro']
      sigma<- p['sigma']/sqrt(2*p['Ro']+ p['theta'])
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
                            t1=times[2:n], pars, log=T))
      out
  }
  o <- optim(p, f, ...)

  if(!store_data) # remove the data object to save space?
    X <- NULL
  # format the output
  out <- list(data=X, pars=o$par, model=model, loglik = o$value,
       convergence=(o$convergence==0) )
  class(out) <- "gauss"
  out
}


