<!--roptions dev=png,fig.width=5,fig.height=5,warning=FALSE, comment=NA-->


Autocorrelation performs particularly poorly on the examples in the text, falling on approximately the 1:1 line in the ROC curves.  This example goes through the steps to demonstrate that in a sufficiently-frequently sampled timeseries, autocorrelation does contain **some** signal of early warning, it is simply a weaker signal than the corresponding analysis with variance, which in turn is weaker than the model-based approach.  Bear in mind the purpose of this is to highlight this approach to quantifying uncertainty in the performance of warning signals as it is to choose the best indicator.  

<!--begin.rcode setup, echo=FALSE
render_gfm() # use GFM hooks for output
opts_knit$set(base.url='https://github.com/cboettig/earlywarning/wiki/')
end.rcode-->


<!--begin.rcode libraries, echo=TRUE, include=FALSE
require(populationdynamics) 
require(earlywarning)
require(ggplot2)
require(reshape2)
require(plyr)
require(beanplot)
require(doMC)
end.rcode-->

We begin by running the individual based simulation, supplied in our helper package, populationdynamics.  

<!--begin.rcode simulation
require(populationdynamics) 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, i = 0, Da = .09, Dt = 0, p = 2)
time=seq(0, 500, length=500)
sn <- saddle_node_ibm(pars,time)
X <- data.frame(time=time, value=sn$x1)
end.rcode-->

We compute the observed value of Kendall's tau for the autocorrelation computed in a moving window over the data.  (The warningtrend function is just a wrapper for the base function cor.test, which handles different window sizes.  The default uses half the length of the timeseries.)  

<!--begin.rcode observed
observed <- warningtrend(X, window_autocorr) 
observed
end.rcode-->

While this gives us single value which is useful for statistical comparisons that follow, recall that it is more common to simply plot the autocorrelation computed in this manner, and the increase is just the visual pattern.  

<!--begin.rcode plotautocor
npts <- length(X[["value"]])
autocorrelation <- window_autocorr(X[["value"]], windowsize=npts/2)
autocorrelation <- c(rep(NA, npts/2), autocorrelation)
plot(X[[time]], autocorrelation, type="l")
end.rcode-->

<!-- I don't think this helps, 
 fit the models

 * \( dX = \alpha(\theta - X) dt + \sigma dB_t \)
 
 * \( dX = \gamma_t (\theta - X) dt + \sigma\sqrt(\gamma_t+\theta) dB_t \)
-->


To bootstrap the estimate of tau, we need to be able to simulate under a null and test model.  We use our models of a stable system and a system approaching a collapse to do this.  We first estimate the model parameters from the data, 

<!--begin.rcode fit 
A <- stability_model(X, "OU")
B <- stability_model(X, "LSN")
end.rcode-->

and then we can simulate some replicates

<!--begin.rcode simulate
reps <- 100
Asim <- simulate(A, reps)
Bsim <- simulate(B, reps)
end.rcode-->


tidy up the data a bit; columns should be variables, not replicates [Wickam (2007)](http://www.jstatsoft.org/v21/i12/).  
<!--begin.rcode tidy
Asim <- melt(Asim, id="time")
Bsim <- melt(Bsim, id="time")
names(Asim)[2] <- "rep"
names(Bsim)[2] <- "rep"
end.rcode-->

Now that we have replicates from each process, we can apply the `warningtrend` `window_autocorr` to each replicate
<!--begin.rcode warning
wsA <- ddply(Asim, "rep", warningtrend, window_autocorr)
wsB <- ddply(Bsim, "rep", warningtrend, window_autocorr)
end.rcode-->

And gather and plot the results
<!--begin.rcode plot
tidy <- melt(data.frame(null=wsA$tau, test=wsB$tau))
names(tidy) <- c("simulation", "value")
beanplot(value ~ simulation, tidy, what=c(0,1,0,0))
abline(h=observed, lty=2)
end.rcode-->

<!--begin.rcode roc
roc <- roc_data(tidy)
ggplot(roc) + geom_line(aes(False.positives, True.positives), lwd=1)
end.rcode-->



## Parallelization 

Parallel code for the `plyr` command is straight-forward for multicore use,

<!--begin.rcode parallel
registerDoMC()
wsA <- ddply(Asim, "rep", warningtrend, window_autocorr, .parallel=TRUE)
wsB <- ddply(Bsim, "rep", warningtrend, window_autocorr, .parallel=TRUE)
end.rcode-->
Which works nicely (other than the progress indicator finishing early).





In principle, this can be [parallelized over MPI](http://stackoverflow.com/questions/5559287/how-do-i-make-dosmp-play-nicely-with-plyr) using 
[an additional function](http://www.numbertheory.nl/2011/11/14/parallelization-using-plyr-loading-objects-and-packages-into-worker-nodes/), seems to work:

<!--begin.rcode parallel_mpi 
library(snow)
library(doSNOW)
source("../createCluster.R")
cl <- createCluster(4, export = ls(), lib = list("earlywarning"))
ws <- ddply(Asim, "rep", warningtrend, window_autocorr, .parallel=TRUE)
stopCluster(cl)
head(ws)
end.rcode-->

