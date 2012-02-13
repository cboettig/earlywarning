
# gauss class has:
# data (data.frame), 
# parameters (named vector numeric), 
# model-name (character), 
# log-likelihood (scalar numeric)
# convergence (logical)
# 

#' converge generic test
#' 
#' @param object a class of model fit
#' @param ... additional arguments 
#' @return convergence status (logical)
#' @export 
converge <- function(object, ...) UseMethod("converge")

#' Test if a gaussian model fit has converged
#' 
#' @param object a class of model fit
#' @param ... additional arguments 
#' @return convergence status (logical)
#' @S3method converge gauss
#' @method converge gauss
converge.gauss <- function(object, ...){
  object$convergence
}

#' extract the fitted model parameters
#' 
#' @param object a gauss class object
#' @param ... aditional arguments (currently ignored)
#' @return a (named) vector of the MLE parameters
#' @S3method fitted gauss
#' @method fitted gauss
fitted.gauss <- function(object, ...){
  object$pars
}



#' update routine for gaussian model fits
#' 
#' @param object any gauss-class object
#' @param ... additional arguments, see details.  Must at least include X=
#' @return an updated gauss-class object depending on other parameters given
#' @details data has to be passed in as X= via the ..., see example.
#' any commands to the optim routine used are also passed in in this manner.
#' one can also add the option store_data = FALSE to avoid returning the 
#' input data, see \code{\link{stability_model}} for details.  
#' @S3method update gauss
#' @method update gauss
update.gauss <- function(object, ...){
  stability_model(..., model = object$model, p = object$pars)
}

#' simulate method
#' 
#' @param object any gauss-class object
#' @param ... additional arguments (currently ignored)
#' @return a data frame with the time simulated values
#' @S3method simulate gauss
#' @method simulate gauss
simulate.gauss <- function(object, ...){
  if(object$model == "LSN") 
    setmodel <- LSN
  else if(object$model == "OU") 
    setmodel <- constOU
   times <- object$X[,1]
   N <- length(times)
   X <- numeric(N - 1)
   X[1] <- object$X[1,2]
   for(i in 1:(N - 1) ){
     X[i + 1] <- rc.gauss(setmodel, 1, x0 = X[i], to = times[i], 
                        t1 = times[i + 1], object$pars)
   }
   data.frame(times, X)
}


#' extract the logLik 
#' 
#' @param object a gauss class model fit
#' @return the log likelihood (not the negative log lik, & not 2*log likelihood)
#' @details note that this is an extraction method, not a calculation method
#' @S3method logLik gauss
#' @method logLik gauss
logLik.gauss <- function(object, ...){
  object$loglik
}


#' random number draw from a gauss model
#' 
#' @param setmodel a function returning mean and variance for 
#' the desired model class
#' @param n replicates
#' @param x0 the initial value 
#' @param to initial time
#' @param t1 end time
#' @param pars parameters passed to setmodel function
#' @return a normal random variate with mean and sd determined by setmodel
#' @details (called internally by full names despite being written 
#' in S3-style notation)
rc.gauss <- function(setmodel, n=1, x0, to, t1, pars){
    P <- setmodel(x0, to, t1, pars)
      rnorm(n, mean=P$Ex, sd=sqrt(P$Vx))
}

#' probability density 
#' 
#' @param setmodel a function returning mean and variance for the 
#' desired model class
#' @param x0 the initial value 
#' @param to initial time
#' @param t1 end time
#' @param pars parameters passed to setmodel function
#' @param log a logical indicating whether log of the density is desired
#' @return the probability density for the given interval 
dc.gauss  <- function(setmodel, x, x0, to, t1, pars, log = FALSE){
      P <- setmodel(x0, to, t1, pars)
          dnorm(x, mean=P$Ex, sd=sqrt(P$Vx), log=log)
}

#' cumulative density function
#'
#' @param setmodel a function returning mean and variance for
#' the desired model class
#' @param x0 the initial value 
#' @param to initial time
#' @param t1 end time
#' @param pars parameters passed to setmodel function
#' @param lower.tail logical 
#' @param log.p a logical indicating whether log of the density is desired
#' @return the cumulative probability density for the given interval 
pc.gauss  <- function(setmodel, x, x0, to, t1, pars,
                      lower.tail = TRUE, log.p = FALSE){ 
    P <- setmodel(x0, to, t1, pars)
    pnorm(x, mean=P$Ex, sd=sqrt(P$Vx),
          lower.tail = lower.tail, log.p = log.p)
}




