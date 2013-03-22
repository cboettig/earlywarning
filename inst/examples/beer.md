

# Code for Prosecutors Fallacy 

For the individual-based simulation, the population dynamics are given by

<div>
\begin{align}
  \frac{dP(n,t)}{dt} &= b_{n-1} P(n-1,t) + d_{n+1}P(n+1,t) - (b_n+d_n) P(n,t)  \label{master}, \\
    b_n &= \frac{e K n^2}{n^2 + h^2}, \\
    d_n &= e n + a,
\end{align}
</div>

which is provided by the `saddle_node_ibm` model in `populationdynamics`. 

For each of the warning signal statistics in question, 
we need to generate the distibution over all replicates
and then over replicates which have been selected conditional 
on having experienced a crash.  

We begin by running the simulation of the process for all replicates.  

Load the required libraries
 

```r
rm(list=ls())
library(populationdynamics)
library(earlywarning)
library(reshape2)		# data manipulation
library(data.table)	# data manipulation
library(ggplot2)		# graphics
library(snowfall)		# parallel
```



```r
theme_publish <- theme_set(theme_bw(12))
theme_publish <- 
  theme_update(legend.key=theme_blank(),
        panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),
        plot.background=theme_blank(), legend.title=theme_blank())
```



### Conditional distribution

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 50,000 pts a piece, we can save memory by performing the conditional selection on the ones that cross a threshold we go along and disgard the others.  (We will create a null distribution in which we ignore this conditional selection later).  



```r
threshold <- 220
pars = c(Xo = 500, e = 0.5, a = 0, K = 500, h = 200, i = 0, Da = 0, Dt = 0, p = 1)
sn <- saddle_node_ibm(pars, times=seq(0,5000, length=50000), reps=2000)
timeseries <- sn$x1
# Get the ids of samples that have a point less than the theshold
w <- sapply(data.frame(timeseries), function(x) any(x < threshold))
# extract that subset by id 
W <- timeseries[,w]
# Get the range of points 200 samples before the smallest deviation
dev <- sapply(as.data.frame(W), which.min)
# Drop transitions with less than 200 observations
drop <- which(dev - 200 < 1)
if(length(drop) > 0){
W <- W[,-drop]
dev <- dev[-drop]
}
D <- rbind((dev - 200), dev)
# extract just that range
M <- as.matrix(W)
dat <- sapply(1:length(dev), function(i) M[D[1,i]:D[2,i], i])
dat <- as.data.frame(dat)
dat <- cbind(time = 1:dim(dat)[1], dat)
df <- melt(dat, id="time")
names(df) = c("time", "reps", "value")
levels(df$reps) <- 1:length(levels(df$reps)) # use numbers for reps instead of V1, V2, etc
zoom <- df
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8237/8581007416_91e4cc5359_o.png) 


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
null <- timeseries[1000:1501,]
null <- cbind(time = 1:dim(null)[1], null)
ndf <- melt(null, id="time")
names(ndf) = c("time", "reps", "value")
levels(ndf$reps) <- 1:length(levels(ndf$reps)) # use numbers for reps instead of V1, V2, etc
nulldt <- data.table(ndf)
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```



```r
ggplot(dat) + geom_histogram(aes(value, y=..density..), binwidth=0.3, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), adjust=2) + xlab("Kendall's tau") + theme_bw()
```

![plot of chunk fig](http://farm9.staticflickr.com/8507/8579908217_88fae4cf87_o.png) 








```r
write.csv(dat, file="beer_dat.csv")
write.csv(nulldat, file="beer_nulldat.csv")
```

