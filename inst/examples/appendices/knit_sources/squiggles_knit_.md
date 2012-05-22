<!--roptions dev=png,fig.width=9,fig.height=6,warning=FALSE,cache.path=squiggles/, comment=NA-->


<!--begin.rcode setup, echo=FALSE
render_gfm() # use GFM hooks for output
opts_knit$set(base.url='https://github.com/cboettig/earlywarning/wiki/')
end.rcode-->

<!-- This loads all the libraries we need ahead of time, so they load quitely when displayed!-->
<!--begin.rcode libraries, echo=FALSE
require(populationdynamics) 
require(earlywarning)
require(plyr)
require(ggplot2)
require(reshape2)
end.rcode-->



Variability in the appearance of a trend 
<!--begin.rcode nreps
nreps <- 64
end.rcode-->

<!--begin.rcode crash
require(populationdynamics) 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, i = 0, Da = .09, Dt = 0, p = 2)
time=seq(0, 990, length=500)
sn <- saddle_node_ibm(pars,time, reps=nreps)
end.rcode-->

We reformat the replicates into long form, 
<!--begin.rcode reformat
X <- data.frame(time=time, value=sn$x1)
require(reshape)
dat <- melt(X, id="time")
names(dat)[2] <- "reps"
end.rcode-->


<!--begin.rcode autocor
require(plyr)
window <- length(X[["time"]])/2
tmp <- ddply(dat, "reps", function(X) window_autocorr(X$value, windowsize=window))
end.rcode-->

Tidy up the warning signal data
<!--begin.rcode reformatting
acorr <- melt(t(tmp))
acorr <- acorr[-1,]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
end.rcode-->

and plot the replicate warning signals
<!--begin.rcode crashautocor
require(ggplot2)
ggplot(acorr) + geom_line(aes(time, value)) + facet_wrap(~reps)+ 
  opts(title="Autocorrelation on replicates from a system approaching a crash")

end.rcode-->


## Stable system

Stable model simulations 
<!--begin.rcode stable
pars = c(Xo = 730, e = 0.5, a = 150, K = 1000, h = 200, i = 0, Da = .00, Dt = 0, p = 2)
time=seq(0, 500, length=500)
sn <- saddle_node_ibm(pars,time, reps=nreps)
X <- data.frame(time=time, value=sn$x1)
stable_dat <- melt(X, id="time")
names(stable_dat)[2] <- "reps"
end.rcode-->


<!--begin.rcode stableautocor
tmp <- ddply(stable_dat, "reps", function(X) window_autocorr(X$value, windowsize=window))
acorr <- melt(t(tmp))
acorr <- acorr[-1,]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + facet_wrap(~reps)+ 
  opts(title="Autocorrelation on replicates from a stable system")

end.rcode-->



## Variance pattern


### Replicates approaching a crash

<!--begin.rcode crashvar
tmp <- ddply(dat, "reps", function(X) window_var(X$value, windowsize=window))
acorr <- melt(t(tmp))
acorr <- acorr[-1,]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + facet_wrap(~reps) + 
  opts(title="Variance on replicates approaching a crash")
end.rcode-->


### Replicates from a stable system 

<!--begin.rcode stablevar
tmp <- ddply(stable_dat, "reps", function(X) window_var(X$value, windowsize=window))
acorr <- melt(t(tmp))
acorr <- acorr[-1,]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + facet_wrap(~reps) + 
  opts(title="Variance on replicates in a stable system")
end.rcode-->



