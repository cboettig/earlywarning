

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

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 50,000 pts a piece, we can save memory by performing the conditional selection on the ones that crash as we go along and disgard the others.  (We will create a null distribution in which we ignore this conditional selection later).  



```r
threshold <- 250
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





To take advantage of parallelization, we loop over this function a set number of times.  The `snowfall` library provides the parallelization
of the `lapply` loop.  A few extra commands format the data into a table
with columns of times, replicate id number, and population value at the
given time.



```r
sfInit(parallel=TRUE, cpu=16)
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




```r
ggplot(subset(dat, reps %in% levels(dat$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk testing](http://farm9.staticflickr.com/8450/7940301598_4938de5109_o.png) 



Zoom in on the relevant area of data near the crash


```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value<threshold))
print(tip)
    index <- max(tip-200,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
```

```
[1] 5276
[1] 43428
[1] 5276
[1] 5276
[1] 5276
[1] 48252
[1] 5276
[1] 5276
[1] 5276
[1] 5276
[1] 5276
[1] 32830
[1] 5276
[1] 5276
[1] 5276
[1] 5276
[1] 5276
[1] 32830
[1] 5276
[1] 5276
[1] 33345
[1] 34821
[1] 33345
[1] 33345
[1] 33345
[1] 4985
[1] 33345
[1] 33345
[1] 33345
[1] 33345
[1] 33345
[1] 15238
[1] 33345
[1] 33345
[1] 33345
[1] 33345
[1] 33345
[1] 15238
[1] 33345
[1] 33345
[1] 38063
[1] 21045
[1] 38063
[1] 38063
[1] 38063
[1] 49751
[1] 38063
[1] 38063
[1] 38063
[1] 38063
[1] 38063
[1] 22679
[1] 38063
[1] 38063
[1] 38063
[1] 38063
[1] 38063
[1] 22679
[1] 38063
[1] 38063
[1] 26790
[1] 38952
[1] 26790
[1] 26790
[1] 26790
[1] 38005
[1] 26790
[1] 26790
[1] 26790
[1] 26790
[1] 26790
[1] 2822
[1] 26790
[1] 26790
[1] 26790
[1] 26790
[1] 26790
[1] 2822
[1] 26790
[1] 26790
[1] 2190
[1] 25391
[1] 40879
[1] 25391
[1] 1643
[1] 30177
[1] 44623
[1] 30177
[1] 43667
[1] 4420
[1] 14053
[1] 18017
[1] 42560
[1] 44897
[1] 5272
[1] 7789
[1] 6299
[1] 7789
[1] 7789
[1] 7789
[1] 6100
[1] 7789
[1] 7789
[1] 7789
[1] 7789
[1] 7789
[1] 3034
[1] 7789
[1] 7789
[1] 7789
[1] 7789
[1] 7789
[1] 3034
[1] 7789
[1] 7789
[1] 30779
[1] 40846
[1] 5547
[1] 41192
[1] 27208
[1] 41192
[1] 41192
[1] 41192
[1] 46567
[1] 41192
[1] 41192
[1] 41192
[1] 41192
[1] 41192
[1] 47378
[1] 41192
[1] 41192
[1] 41192
[1] 41192
[1] 41192
[1] 47378
[1] 41192
[1] 41192
[1] 5277
[1] 20996
[1] 5277
[1] 5277
[1] 5277
[1] 44952
[1] 5277
[1] 5277
[1] 5277
[1] 5277
[1] 5277
[1] 5168
[1] 5277
[1] 5277
[1] 5277
[1] 5277
[1] 5277
[1] 5168
[1] 5277
[1] 5277
[1] 31543
[1] 28517
[1] 31543
[1] 31543
[1] 31543
[1] 8558
[1] 31543
[1] 31543
[1] 31543
[1] 31543
[1] 31543
[1] 45461
[1] 31543
[1] 31543
[1] 31543
[1] 31543
[1] 31543
[1] 45461
[1] 31543
[1] 31543
[1] 17554
[1] 34648
[1] 17554
[1] 17554
[1] 17554
[1] 14403
[1] 17554
[1] 17554
[1] 17554
[1] 17554
[1] 17554
[1] 34460
[1] 17554
[1] 17554
[1] 17554
[1] 17554
[1] 17554
[1] 34460
[1] 17554
[1] 17554
[1] 39601
[1] 14011
[1] 39601
[1] 39601
[1] 39601
[1] 13663
[1] 39601
[1] 39601
[1] 39601
[1] 39601
[1] 39601
[1] 42877
[1] 39601
[1] 39601
[1] 39601
[1] 39601
[1] 39601
[1] 42877
[1] 39601
[1] 39601
[1] 42521
[1] 14638
[1] 42521
[1] 42521
[1] 42521
[1] 29132
[1] 42521
[1] 42521
[1] 42521
[1] 42521
[1] 42521
[1] 38393
[1] 42521
[1] 42521
[1] 42521
[1] 42521
[1] 42521
[1] 38393
[1] 42521
[1] 42521
[1] 46875
[1] 21334
[1] 46875
[1] 46875
[1] 46875
[1] 3816
[1] 46875
[1] 46875
[1] 46875
[1] 46875
[1] 46875
[1] 41375
[1] 46875
[1] 46875
[1] 46875
[1] 46875
[1] 46875
[1] 41375
[1] 46875
[1] 46875
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8447/7940301788_a2d83a1b3e_o.png) 



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
threshold <- 250
select_crashes <- function(n){
	T<- 5000
	n_pts <- n
	pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
	sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=500)
	d <- dim(sn$x1)
	sn$x1[1:501,]
}
```




```r
sfInit(parallel=TRUE, cpu=16)
sfLibrary(populationdynamics)
```

```
Library populationdynamics loaded.
```

```r
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



```r
nulldt <- data.table(nullzoom)
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```



```r
ggplot(dat) + geom_histogram(aes(value, y=..density..), binwidth=0.3, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), adjust=2) + xlab("Kendall's tau") + theme_bw()
```

![plot of chunk fig2](http://farm9.staticflickr.com/8441/7940301950_8e4e8a4105_o.png) 







