
#' fit both constant and time-dependent model
#' 
#' Estimate decent starting parameter values from data & fits both models
#' @param X a timeseries object (or matrix or data-frame)
#' @param ... additional options taht are passed to the optimizer
#' @return the constant and time-dependent model estimates, 
#' the data input, and the initial guesses of the parameters.
#' @export
fit_models <- function(X,  ...){
  const_pars <- c(Ro=as.numeric(1/max(time(X))), 
                 theta=as.numeric(mean(X)), sigma=as.numeric(sd(X)))
  const <- updateGauss(constOU, const_pars, X, ...)
  if(const$optim_output$convergence!=0){
   message(const$optim_output$message)
    stop("Constant model did not converge, 
          please try different optimizer settings")
  }
  # guess LSN parameters from OU parameterization
  guess_Ro <- as.numeric(const$pars['Ro']^2)
  guess_theta <- as.numeric(const$pars['theta']+const$pars['Ro'] )
  guess_sigma<- as.numeric(const$pars['sigma']/sqrt(2*const$pars['Ro']+
                               const$pars['theta']))
  pars <- c(Ro=guess_Ro, m=0, theta=guess_theta, sigma=guess_sigma)
  timedep <- updateGauss(timedep_LSN, pars, X, ...)

  ## Check convergence of each model
  const$optim_output$convergence
  if(timedep$optim_output$convergence!=0){
   message(timedep$optim_output$message)
    stop("Time dependent model did not converge, 
          please try different optimizer settings")
  }
  # Outputs in a named list
  list(X=X, const=const, timedep=timedep, pars=pars,
       const_pars=const_pars)
}
