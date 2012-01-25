constOU <- function(Xo, to, t1, pars){
  Dt <- t1 - to
  Ex <- pars["theta"]*(1 - exp(-pars["Ro"]*Dt)) + Xo*exp(-pars["Ro"]*Dt) 
  Vx <- 0.5*pars["sigma"]^2 *(1-exp(-2*pars["Ro"]*Dt))/pars["Ro"]
  if(pars['Ro'] < 0 ) Vx <- rep(Inf, length(Xo)) 
  if(pars['sigma'] < 0 ) Vx <- rep(Inf, length(Xo)) 
  return(list(Ex=Ex, Vx=Vx))
}




