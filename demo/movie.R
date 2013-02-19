# saddle_analytics.R
params <- c(K=1000, e=.5, h=200, p=2)
b <- function(x, params){ 
  params["e"] * params["K"] * x ^ params["p"] / (x ^ params["p"] + params["h"] ^ params["p"]) 
}
d <- function(x, a, params){ 
  params["e"] * x + a
}

#d <- function(x, a, params) { a*(x-params["K"])+params["e"]*params["K"] }

lambda <- function(x, a, params){
  params["p"] * params["e"] * params["K"] * x ^ (params["p"] - 1) / (x ^ params["p"] + params["h"] ^ params["p"]) - pparams["p"] * params["e"] * params["K"] * x ^ (2 * params["p"] - 1) / (x ^ params["p"] + params["h"] ^ params["p"]) ^ 2 - params["e"] }


x <- seq(0, 1300, by=5)
a<- 1

#enviro_change <- seq(1,0, length=10)
enviro_change <- seq(0,220, length=100)


bifur <- function()
for(a in enviro_change){
  plot(x, b(x,params) - d(x,a, params), type="l", ylim=c(-150, 900), lwd=2)
  lines(x, b(x,params), lwd=4, lty=1, col="darkblue")
  lines(x, d(x,a, params), lwd=4, lty=1, col="darkred")
  abline(h=0)
}

require(animation)
saveMovie(bifur(), interval=.05, loop=1, outdir=getwd())
