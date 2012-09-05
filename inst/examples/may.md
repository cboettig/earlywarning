


## Code for the May Model 

<div>
\begin{equation}
X_{t+1} =     X_t  \exp\left( r \left(1 - \frac{ X_t }{  K } \right) - \frac{ a * X_t ^ {Q - 1} }{X_t ^ Q + H ^ Q} \right) 
\end{equation}
</div>


We will use parameters r = .75, k = 10, a=1.7, H=1, Q = 3.  In this model Q is a parameter that will force the system through a bifurcation point at a = 2.  

For each of the warning signal statistics in question, 
we need to generate the distibution over all replicates
and then over replicates which have been selected conditional 
on having experienced a crash.  


Load the required libraries
 

```r
library(earlywarning)
library(reshape2)		# data manipulation
library(data.table)	# data manipulation
library(ggplot2)		# graphics
library(snowfall)		# parallel
```


This just sets our plotting preferences


```r
theme_publish <- theme_set(theme_bw(12))
theme_publish <- 
  theme_update(legend.key=theme_blank(),
        panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),
        plot.background=theme_blank(), legend.title=theme_blank())
```




### Conditional distribution

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 5,000 pts a piece, we can save memory by performing the conditional selection on the ones that crash as we go along and disgard the others.  


```r
threshold <- 1.5
select_crashes <- function(n){
  n <- n/10 # doesn't need as long as the individual-based
  sn <- 
  sapply(1:1000, function(rep){
    x <- vector(mode="double", length=n)
    x[1] <- 8 # positive equilibrium
    z <- rlnorm(n, 0, .1)
    r = .75; k = 10; a=1.55; H=1; Q = 3
    for(t in 1:n){
      x[t+1] = z[t] *  x[t] * exp(r * (1 - x[t] / k) - a * x[t] ^ (Q - 1) / (x[t] ^ Q + H ^ Q)) 
    }
    x
  })
	crashed <- which(sn[n,] < threshold)
	sn[,crashed] 
}
```



Initialize the parallel enviromment and run the simulations:


```r
sfInit(parallel=TRUE, cpu=12)
```

```
R Version:  R version 2.15.0 (2012-03-30) 

```

```r
sfExportAll()
examples <- sfLapply(1:20, function(i) select_crashes(50000))
dat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
names(dat) = c("time", "reps", "value")
levels(dat$reps) <- 1:length(levels(dat$reps)) # use numbers for reps
```




```r
ggplot(subset(dat, reps %in% levels(dat$reps)[1:9])) + 
  geom_line(aes(time, value)) +
  facet_wrap(~reps, scales="free")
```

![plot of chunk testing](http://farm9.staticflickr.com/8316/7939240546_bf7fe06278_o.png) 



Zoom in on the relevant area of data near the crash


```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value<threshold))
    index <- max(tip-200,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + 
  geom_line(aes(time, value)) + 
  facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8304/7939240812_b9640474a6_o.png) 



Compute model-based warning signals on all each of these.  


```r
dt <- data.table(subset(zoom, value>threshold))
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))
```


### Null distribution 

To compare against the expected distribution of these statistics, we create another set of simulations without conditioning on having experienced a chance transition, on which we perform the identical analysis.  


```r
select_crashes <- function(n){
  n <- n/10 # doesn't need as long as the individual-based
  sn <- 
  sapply(1:1000, function(rep){
    x <- vector(mode="double", length=n)
    x[1] <- 8 # positive equilibrium
    z <- rlnorm(n, 0, .1)
    r = .75; k = 10; a=1.55; H=1; Q = 3
    for(t in 1:n){
      x[t+1] = z[t] *  x[t] * exp(r * (1 - x[t] / k) - a * x[t] ^ (Q - 1) / (x[t] ^ Q + H ^ Q)) 
    }
    x
  })
	sn[1:201,] 
}
```






```r
sfLibrary(populationdynamics)
sfExportAll()
examples <-  sfLapply(1:10, function(i) select_crashes(50000))
nulldat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
nulldat <- melt(examples)

names(nulldat) = c("time", "reps", "value")
levels(nulldat$reps) <- 1:length(levels(dat$reps)) 
```


Zoom in on the relevant area of data near the crash


```r
require(plyr)
nullzoom <- ddply(nulldat, "reps", function(X){
    data.frame(time=X$time, value=X$value)
    })
```


Compute the warning signals on the null distribution for comparison:


```r
nulldt <- data.table(nullzoom)
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```


Plot the final figure:


```r
ggplot(dat) + 
	geom_histogram(aes(value, y=..density..), binwidth=0.3, alpha=.5) +
	facet_wrap(~variable) + xlim(c(-1, 1)) + 
	geom_density(data=nulldat, aes(value), adjust=3) + 
	xlab("Kendall's tau") + theme_bw()
```

![plot of chunk figure2](http://farm9.staticflickr.com/8038/7939241062_f602c8647b_o.png) 

```r

```







