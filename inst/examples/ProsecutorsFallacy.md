


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
```



Run the individual based simulation


```r
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1000)
```



```
Error: could not find function "saddle_node_ibm"
```



```r
save("sn", "file=prosecutor.rda")
```



```
Error: objects 'sn', 'file=prosecutor.rda' not found
```






```r
load("prosecutor.rda")
```



```
Error: cannot open the connection
```



```r
d <- dim(sn$x1)
```



```
Error: object 'sn' not found
```



```r
crashed <- which(sn$x1[d[1],]==0)
```



```
Error: object 'sn' not found
```



```r
dat <- melt( sn$x1[,crashed] )
```



```
Error: could not find function "melt"
```



```r
names(dat) = c("time", "reps", "value")
```



```
Error: object 'dat' not found
```



```r
save("dat", file="crashed.rda")
```



```
Error: object 'dat' not found
```




Zoom in on the relevant area of data near the crash



```r
load("crashed.rda")
```



```
Error: cannot open the connection
```



```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value==0))
    index <- max(tip-500,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
```



```
Error: object 'dat' not found
```



```r
zoom <- subset(zoom, value > 300)
```



```
Error: object 'zoom' not found
```



```r
save("zoom", file="zoom.rda")
```



```
Error: object 'zoom' not found
```






```r
load("zoom.rda")
```



```
Error: cannot open the connection
```



```r
require(ggplot2)
ggplot(subset(zoom, reps < 10)) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```



```
Error: object 'zoom' not found
```




Compute model-based warning signals on all each of these.  
Computationally intensive, so we run this section on a cluster of 60 processors.  



```r
load("zoom.rda")
```



```
Error: cannot open the connection
```



```r
L <- length(unique(zoom$reps))
```



```
Error: object 'zoom' not found
```



```r
library(snow)
cluster <- makeCluster(60, type="MPI")
```



```
Error: the `Rmpi' package is needed for MPI clusters.
```



```r
clusterEvalQ(cluster, library(earlywarning)) # load a library
```



```
Error: object 'cluster' not found
```



```r
clusterExport(cluster, ls()) # export everything in workspace
models <- parLapply(cluster, 1:L, function(i)
  stability_model(zoom[zoom$rep==i, c("time", "value")], "LSN")
)
```



```
Error: object 'cluster' not found
```



```r
stopCluster(cluster)
```



```
Error: object 'cluster' not found
```



```r
save("models", file="models.rda")
```



```
Error: object 'models' not found
```




Load the resulting data and compute indicators:



```r
load("zoom.rda")
```



```
Error: cannot open the connection
```



```r
load("models.rda")
```



```
Error: cannot open the connection
```



```r
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



```
Error: object 'zoom' not found
```




Plot distribution of indicators



```r
require(reshape2)
dat <- melt(indicators, id="reps")
```



```
Error: object 'indicators' not found
```



```r
ggplot(subset(dat, variable != "m.m")) + geom_histogram(aes(value)) + facet_wrap(~variable)
```



```
Error: object 'dat' not found
```







```r
require(beanplot)
beanplot(value ~ variable, data=dat, what=c(0,1,0,0), bw="nrd0")
```



```
Error: could not find function "beanplot"
```



```r
save(list=ls(), file="~/public_html/data/ProsecutorsFallacy.rda")
```








