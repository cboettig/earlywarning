
#' OU model for stable system
#' 
#' Estimate the appropriate mean and variance of the OU process
#' @param Xo initial condition
#' @param to initial time
#' @param t1 final time
#' @param pars numeric of parameters named Ro, theta, and sigma
#' @return a list with values Ex, the expected x value, and 
#' Vx, the expected variance
#' @keywords internal
constOU <- function(Xo, to, t1, pars){
  Dt <- t1 - to
  Ex <- pars["theta"]*(1 - exp(-pars["Ro"] * Dt)) + Xo *
        exp(-pars["Ro"] * Dt) 
  Vx <- 0.5 * pars["sigma"] ^ 2 * 
       (1 - exp(-2 * pars["Ro"] * Dt)) / pars["Ro"]
  if(pars['Ro'] < 0 ) Vx <- rep(Inf, length(Xo)) 
  if(pars['sigma'] < 0 ) Vx <- rep(Inf, length(Xo)) 
  return(list(Ex = Ex, Vx = Vx))
}




