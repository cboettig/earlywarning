``` {r echo=FALSE}
#opts_knit$restore()
render_gfm()
opts_chunk$set(warning=FALSE, message=FALSE, comment=NA, tidy=FALSE, fig.path='figures/', cache=TRUE, verbose=TRUE, refresh=4) 
#opts_knit$set(upload.fun = socialR::flickr.url)
opts_chunk$set(dev='png', cache.path='cache-upload/')
#opts_chunk$set(dev='pdf', cache.path='cache-pdf/')
````

# Code for Prosecutors Fallacy 

Simulate a dataset from the full individual, nonlinear model, with stable parameters (*.e.g.* not approaching a bifurcation).

``` {r }
rm(list=ls())
T<- 5000
n_pts <- 50000
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
require(populationdynamics)
require(earlywarning)
require(reshape2)
````
Run the individual based simulation
``` {r }
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=10000)
save("sn", file="prosecutor.rda")
````

``` {r }
load("prosecutor.rda")
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)
dat <- melt( sn$x1[,crashed] )
names(dat) = c("time", "reps", "value")
save("dat", file="crashed.rda")
````

Zoom in on the relevant area of data near the crash

``` {r }
load("crashed.rda")
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value==0))
    index <- max(tip-500,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
zoom <- subset(zoom, value > 300)
save("zoom", file="zoom.rda")
````

``` {r replicate_crashes}
load("zoom.rda")
require(ggplot2)
ggplot(subset(zoom, reps < 10)) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
````

Compute model-based warning signals on all each of these.  
Computationally intensive, so we run this section on a cluster of 60 processors.  

``` {r }
load("zoom.rda")
L <- length(unique(zoom$reps))
L
````


``` {r parallel}
library(snowfall)
sfInit(par=T, cpu=16)
sfLibrary(earlywarning) # load a library
sfExportAll()
models <- sfLapply(unique(zoom$rep), function(i)
  try(stability_model(zoom[zoom$rep==i, c("time", "value")], "LSN"))
)
````

``` {r }
save("models", file="models.rda")
````

Load the resulting data and compute indicators:

``` {r  refresh=2 }
load("zoom.rda")
load("models.rda")
require(plyr)
require(earlywarning)
indicators <- ddply(zoom, "reps", function(X){
    Y <- data.frame(time=X$time, value=X$value)
    tau_var <- warningtrend(Y, window_var)
    tau_acorr <- warningtrend(Y, window_autocorr)
    i <- X$rep[1]
    m <- models[[i]]$pars["m"]
    c(var=tau_var, acor=tau_acorr, m=m)
})
````

Plot distribution of indicators

``` {r indicator_plot, fig.height=3, fig.width=5, refresh=2 }
require(reshape2)
dat <- melt(indicators, id="reps")
ggplot(subset(dat, variable != "m.m")) + geom_histogram(aes(value)) + facet_wrap(~variable)
````


``` {r beanplot, fig.height=4, fig.width=5, refresh=2}
require(beanplot)
beanplot(value ~ variable, data=dat, what=c(0,1,0,0), bw="nrd0")
save(list=ls(), file="~/public_html/data/ProsecutorsFallacy.rda")
````





