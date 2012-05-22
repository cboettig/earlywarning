





<!-- This loads all the libraries we need ahead of time, so they load quitely when displayed!-->





Variability in the appearance of a trend 


```r
nreps <- 64
```






```r
require(populationdynamics)
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, 
    h = 200, i = 0, Da = 0.09, Dt = 0, p = 2)
time = seq(0, 990, length = 500)
sn <- saddle_node_ibm(pars, time, reps = nreps)
```




We reformat the replicates into long form, 


```r
X <- data.frame(time = time, value = sn$x1)
require(reshape)
dat <- melt(X, id = "time")
names(dat)[2] <- "reps"
```







```r
require(plyr)
window <- length(X[["time"]])/2
tmp <- ddply(dat, "reps", function(X) window_autocorr(X$value, 
    windowsize = window))
```




Tidy up the warning signal data


```r
acorr <- melt(t(tmp))
acorr <- acorr[-1, ]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
```




and plot the replicate warning signals


```r
require(ggplot2)
ggplot(acorr) + geom_line(aes(time, value)) + 
    facet_wrap(~reps) + opts(title = "Autocorrelation on replicates from a system approaching a crash")
```

![plot of chunk crashautocor](https://github.com/cboettig/earlywarning/wiki/crashautocor.png) 



## Stable system

Stable model simulations 


```r
pars = c(Xo = 730, e = 0.5, a = 150, K = 1000, 
    h = 200, i = 0, Da = 0, Dt = 0, p = 2)
time = seq(0, 500, length = 500)
sn <- saddle_node_ibm(pars, time, reps = nreps)
X <- data.frame(time = time, value = sn$x1)
stable_dat <- melt(X, id = "time")
names(stable_dat)[2] <- "reps"
```







```r
tmp <- ddply(stable_dat, "reps", function(X) window_autocorr(X$value, 
    windowsize = window))
acorr <- melt(t(tmp))
acorr <- acorr[-1, ]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + 
    facet_wrap(~reps) + opts(title = "Autocorrelation on replicates from a stable system")
```

![plot of chunk stableautocor](https://github.com/cboettig/earlywarning/wiki/stableautocor.png) 




## Variance pattern


### Replicates approaching a crash



```r
tmp <- ddply(dat, "reps", function(X) window_var(X$value, 
    windowsize = window))
acorr <- melt(t(tmp))
acorr <- acorr[-1, ]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + 
    facet_wrap(~reps) + opts(title = "Variance on replicates approaching a crash")
```

![plot of chunk crashvar](https://github.com/cboettig/earlywarning/wiki/crashvar.png) 



### Replicates from a stable system 



```r
tmp <- ddply(stable_dat, "reps", function(X) window_var(X$value, 
    windowsize = window))
acorr <- melt(t(tmp))
acorr <- acorr[-1, ]
names(acorr) <- c("time", "reps", "value")
acorr$time <- as.numeric(gsub("\\D", "", acorr$time))
class(acorr$value) <- "numeric"
ggplot(acorr) + geom_line(aes(time, value)) + 
    facet_wrap(~reps) + opts(title = "Variance on replicates in a stable system")
```

![plot of chunk stablevar](https://github.com/cboettig/earlywarning/wiki/stablevar.png) 




