

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
    index <- max(tip-100,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
zoom <- subset(zoom, value > 300)

save(list="zoom", file="zoom.rda")
```




A plot of the first 9 datasets over the interval used for the warning signal calculation.



```r
require(ggplot2)
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk replicate_crashes](figure/replicate_crashes.png) 


Compute model-based warning signals on all each of these.  




```r
#L <- length(unique(zoom$reps))#sfLibrary(earlywarning)#sfExportAll()#models <- sfLapply(unique(zoom$rep), function(i)#  try(stability_model(zoom[zoom$rep==i, c("time", "value")], "LSN"))#)
```







```r
require(plyr)
require(earlywarning)
indicators <- ddply(zoom, "reps", function(X){
	try({
    Y <- data.frame(time=X$time, value=X$value)
    tau_var <- warningtrend(Y, window_var)
    tau_acorr <- warningtrend(Y, window_autocorr)
#    i <- X$rep[1]
#    m <- models[[i]]$pars["m"]
    c(var=tau_var, acor=tau_acorr)
	})
})
```



```
Error: Results do not have equal lengths
```



```r

indicators <- indicators[sapply(indicators, function(x) !is(x, "try-error"))]
```



```
Error: object 'indicators' not found
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
#ggplot(subset(dat, variable != "m.m")) + geom_histogram(aes(value)) + facet_wrap(~variable)
ggplot(dat) + geom_histogram(aes(value)) + facet_wrap(~variable)
```



```
Error: At least one layer must contain all variables used for facetting
```




Beanplot version of the indicators



```r
require(beanplot)
beanplot(value ~ variable, data=dat, what=c(0,1,0,0), bw="nrd0")
```



```
Error: object 'variable' not found
```



```r
save(list=ls(), file="ProsecutorsFallacy.rda")
```






