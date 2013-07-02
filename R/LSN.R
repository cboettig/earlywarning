
###########  Linearized Saddle-Node (LSN) Model ##################
#' Linearized Saddle Node model
#'
#' Estimate the mean and variance of the linearized saddle
#' node process (in which the spring constant changes over time)
#' @param Xo initial condition
#' @param to initial time
#' @param t1 final time
#' @param pars numeric of parameters named Ro, theta, and sigma
#' @return a list with values Ex, the expected x value, and 
#' Vx, the expected variance
#' @keywords internal
#' @import deSolve
LSN <- function(Xo, to, t1, pars){

  R <- function(t, pars){pars[1] + pars[2]*t }

  check <- any(R(t1,pars) < 0)
	if(is.na(check) | check | pars[3] < 0){
		Ex <- Xo
		Vx <- rep(Inf,length(Xo))
	} else {
		moments <- function(t,y,p){ 
			sqrtR <- sqrt(R(t,pars)) 
			yd1 <- 2*sqrtR*(sqrtR+pars[3] - y[1]) 
			yd2 <- -2*sqrtR*y[2] + p[4]^2*(sqrtR+pars[3])
			list(c(yd1=yd1, yd2=yd2))
		}
		jacfn <- function(t,y,p){
			sqrtR <- sqrt(R(t,pars)) 
			c(
			-2*sqrtR, 0,
			0, -2*sqrtR
		)}
## The apply calls needed to work with vector inputs as Xo (whole timeseries)
		times <- matrix(c(to, t1), nrow=length(to))
		out <- lapply(1:length(Xo), function(i){
			lsoda(y=c(xhat=Xo[i], sigma2=0), times=times[i,], func=moments, 
			      parms=pars, jacfunc=jacfn) 
		})
		Ex <- sapply(1:length(Xo), function(i) out[[i]][2,2]) # times are in rows, cols are time, par1, par2
		Vx <- sapply(1:length(Xo), function(i) out[[i]][2,3])
	}
# Handle badly defined parameters by creating very low probability returns,
# needed particularly for the L-BFGS-B bounded method, which can't handle Infs 
# and does rather poorly... Worth investigating a better way to handle this.  
# Note errors can apply to some of the timeseries, possibly by estimates of m 
# that force system into bifurcation on later parameters (for which terms  
# become undefined)

	if (pars[4] <= 0){
		warning(paste("sigma=",pars[4]))
		Vx <- rep(Inf, length(Xo))
	}
	if (any(Vx < 0, na.rm=TRUE)){
		warning(paste("discard negative Vx,  "))
		Vx[Vx<0] <- Inf
	}
	return(list(Ex=Ex, Vx=Vx))
}



