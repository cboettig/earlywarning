


# Code for Prosecutors Fallacy 

Simulate a dataset from the full individual, nonlinear model, with stable parameters (*.e.g.* not approaching a bifurcation).



```r
rm(list=ls())
T<- 5000
n_pts <- 50000
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
require(populationdynamics)
require(earlywarning)
require(reshape2)
```



Run the individual based simulation


```r
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1000)
save("sn", file="prosecutor.rda")
```






```r
load("prosecutor.rda")
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)
dat <- melt( sn$x1[,crashed] )
names(dat) = c("time", "reps", "value")
save("dat", file="crashed.rda")
```




Zoom in on the relevant area of data near the crash



```r
load("crashed.rda")
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value==0))
    index <- max(tip-500,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
zoom <- subset(zoom, value > 300)
save("zoom", file="zoom.rda")
```






```r
load("zoom.rda")
require(ggplot2)
ggplot(subset(zoom, reps < 10)) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk replicate_crashes](http://farm6.staticflickr.com/5469/7085787893_e88ca87df9_o.png) 


Compute model-based warning signals on all each of these.  
Computationally intensive, so we run this section on a cluster of 60 processors.  



```r
load("zoom.rda")
length(unique(zoom$reps))
```



```
[1] 14
```









```r
load("zoom.rda")
L <- length(unique(zoom$reps))
library(snowfall)
sfInit(par=T, cpu=16)
sfLibrary(earlywarning) # load a library
```



```
Library earlywarning loaded.
```



```r
sfExportAll()
models <- sfLapply(1:L, function(i)
  try(stability_model(zoom[zoom$rep==i, c("time", "value")], "LSN"))
)
save("models", file="models.rda")
sfStop()
```




Load the resulting data and compute indicators:



```r
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
```




Plot distribution of indicators



```r
require(reshape2)
dat <- melt(indicators, id="reps")
ggplot(subset(dat, variable != "m.m")) + geom_histogram(aes(value)) + facet_wrap(~variable)
```

![plot of chunk indicators](http://farm8.staticflickr.com/7057/7085787505_a780476835_o.png) 





```r
require(beanplot)
beanplot(value ~ variable, data=dat, what=c(0,1,0,0), bw="nrd0")
```

![plot of chunk beanplot](http://farm8.staticflickr.com/7037/6939714566_e4f5bcac80_o.png) 

```r
save(list=ls(), file="~/public_html/data/ProsecutorsFallacy.rda")
```








