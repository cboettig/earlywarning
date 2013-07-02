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
#' @import deSolve
#' @export
stability_model <- function(X, model=c("LSN", "OU"), p = NULL, ..., 
                            store_data=TRUE){
  model <- match.arg(model)
  # reformat time series objects into proper data frames
  if(is(X, "ts"))
    X <- data.frame(as.numeric(time(X)), X@.Data)
  # if time values are not provided
  else if(is.null(dim(X)))
    X <- data.frame(1:length(X), X)
  

  if(!is.null(p)){ ## got everything? then rock & roll
    f1 <- switch(model, 
                LSN = f_closure(X, LSN),
                OU = f_closure(X, constOU))
    o <- optim(p, f1, ...)

  } else if(is.null(p)){ ## oh, need p? try:
    p <- c(Ro=1/max(time(X[,1])), theta=mean(X[,2]), sigma=sd(X[,2]))
    f2 <- f_closure(X, constOU)
    o <- optim(p, f2, ...)

    # if model is "OU", we're done.  otherwise:
    if(model=="LSN"){
      f3 <- f_closure(X, LSN) # switch to the LSN model
      p_est <- o$par  # & use the OU estimated pars as starting guess
      # but rescale them to the new definitions:
      Ro <- as.numeric(p_est[1]^2)
      theta <- as.numeric(p_est[2]+p_est[1])
      sigma <- as.numeric(abs(p_est[3]/sqrt(2*p_est[1]+ p_est[2])))
      p <- c(Ro=Ro, m=0, theta=theta, sigma=sigma)

      ## now fit the LSN model
      o <- optim(p, f3, ...)
    }
  }
  names(X) <- c("time", "value")
  ## Collect the results and we're done
  if(!store_data) # remove the data object to save space?
    X <- NULL
  out <- list(X=X, pars=o$par, model=model, loglik = -o$value,
              convergence=(o$convergence==0) )
  class(out) <- c("gauss", "list")
  out
}


# an internal helper function
f_closure <- function(X, setmodel){
  function(p){
      n <- length(X[,1])
      out <- -sum(dc.gauss(setmodel, X[2:n,2], X[1:(n-1),2], to=X[1:(n-1),1],
                            t1=X[2:n,1], p, log=T))
      if(abs(out) == Inf | is.nan(out))
        out <- 1e19
      out
  }
}


