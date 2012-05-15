

# Code for Prosecutors Fallacy 

Load the required libraries
 


```r
require(populationdynamics)
require(earlywarning)
require(reshape2)
require(snowfall)
```




Simulate a dataset from the full individual, nonlinear model, with stable parameters (*.e.g.* not approaching a bifurcation).

This defines our simulation function



```r
select_crashes <- function(n){
	T<- 5000
	n_pts <- n
	pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
	sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1000)
	d <- dim(sn$x1)
	crashed <- which(sn$x1[d[1],]==0)
	sn$x1[,crashed] 
}
```




To take advantage of parallelization, we loop over this function a set number of times.  



```r
sfInit(parallel=TRUE, cpu=12)
```



```
R Version:  R version 2.15.0 (2012-03-30) 

```



```r
sfLibrary(populationdynamics)
```



```
Library populationdynamics loaded.
```



```r
sfExportAll()
examples <- sfLapply(1:20, function(i) select_crashes(50000))
dat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
names(dat) = c("time", "reps", "value")
levels(dat$reps) <- 1:length(levels(dat$reps)) # use numbers for reps
```




Zoom in on the relevant area of data near the crash



```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value==0))
    index <- max(tip-500,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
save(list="zoom", file="zoom.rda")
```




A plot of the first 9 datasets over the interval used for the warning signal calculation.



```r
require(ggplot2)
ggplot(subset(zoom, value>250 & reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk replicate_crashes](http://farm9.staticflickr.com/8152/7184614268_afeea67382_o.png) 


Compute model-based warning signals on all each of these.  



```r
library(data.table)
library(reshape2)
library(earlywarning)
library(ggplot2)
load("zoom.rda")
dt <- data.table(subset(zoom, value>250))
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(var=var, acor=acor))
ggplot(dat) + geom_histogram(aes(value), binwidth=0.2) + facet_wrap(~variable) + xlim(c(-1, 1))
```

![plot of chunk unnamed-chunk-1](http://farm6.staticflickr.com/5156/7184614770_993cc91a1c_o.png) 


