

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

![plot of chunk testing](http://farm9.staticflickr.com/8177/7939315268_68f4a42866_o.png) 



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
[1] 46194
[1] 27439
[1] 46194
[1] 46194
[1] 46194
[1] 43252
[1] 46194
[1] 46194
[1] 46194
[1] 46194
[1] 46194
[1] 29244
[1] 46194
[1] 46194
[1] 46194
[1] 46194
[1] 46194
[1] 46722
[1] 46194
[1] 46194
[1] 41042
[1] 3193
[1] 41042
[1] 41042
[1] 41042
[1] 16278
[1] 41042
[1] 41042
[1] 41042
[1] 41042
[1] 41042
[1] 16177
[1] 41042
[1] 41042
[1] 41042
[1] 41042
[1] 41042
[1] 20564
[1] 41042
[1] 41042
[1] 8010
[1] 30261
[1] 8010
[1] 8010
[1] 8010
[1] 21705
[1] 8010
[1] 8010
[1] 8010
[1] 8010
[1] 8010
[1] 43712
[1] 8010
[1] 8010
[1] 8010
[1] 8010
[1] 8010
[1] 26407
[1] 8010
[1] 8010
[1] 4618
[1] 153
[1] 4618
[1] 4618
[1] 4618
[1] 25663
[1] 4618
[1] 4618
[1] 4618
[1] 4618
[1] 4618
[1] 5190
[1] 4618
[1] 4618
[1] 4618
[1] 4618
[1] 4618
[1] 11430
[1] 4618
[1] 4618
[1] 45525
[1] 20577
[1] 45525
[1] 45525
[1] 45525
[1] 15468
[1] 45525
[1] 45525
[1] 45525
[1] 45525
[1] 45525
[1] 29270
[1] 45525
[1] 45525
[1] 45525
[1] 45525
[1] 45525
[1] 43847
[1] 45525
[1] 45525
[1] 23185
[1] 47328
[1] 23185
[1] 23185
[1] 36082
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 42121
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 23185
[1] 25589
[1] 24380
[1] 25589
[1] 25589
[1] 42674
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 20122
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 25589
[1] 20102
[1] 9574
[1] 20102
[1] 20102
[1] 41757
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 20102
[1] 26059
[1] 9711
[1] 26059
[1] 26059
[1] 23027
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 26059
[1] 4369
[1] 37174
[1] 4369
[1] 4369
[1] 12862
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 4369
[1] 49925
[1] 4785
[1] 49925
[1] 49925
[1] 41663
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 49925
[1] 10616
[1] 4502
[1] 10616
[1] 10616
[1] 10616
[1] 16391
[1] 10616
[1] 10616
[1] 10616
[1] 10616
[1] 10616
[1] 12275
[1] 10616
[1] 10616
[1] 10616
[1] 10616
[1] 10616
[1] 26572
[1] 10616
[1] 10616
[1] 2504
[1] 37263
[1] 2504
[1] 2504
[1] 43437
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 2504
[1] 6358
[1] 3040
[1] 6358
[1] 6358
[1] 10946
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 6358
[1] 23675
[1] 40167
[1] 23675
[1] 23675
[1] 9474
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 23675
[1] 12649
[1] 12649
[1] 12649
[1] 31286
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 12649
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 18374
[1] 9826
[1] 42557
[1] 9826
[1] 9826
[1] 9826
[1] 5032
[1] 9826
[1] 9826
[1] 9826
[1] 9826
[1] 9826
[1] 47536
[1] 9826
[1] 9826
[1] 9826
[1] 9826
[1] 9826
[1] 12744
[1] 9826
[1] 9826
[1] 10232
[1] 4256
[1] 10232
[1] 10232
[1] 10232
[1] 11877
[1] 10232
[1] 10232
[1] 10232
[1] 10232
[1] 10232
[1] 39661
[1] 10232
[1] 10232
[1] 10232
[1] 10232
[1] 10232
[1] 28841
[1] 10232
[1] 10232
[1] 30097
[1] 12880
[1] 30097
[1] 30097
[1] 30097
[1] 13543
[1] 30097
[1] 30097
[1] 30097
[1] 30097
[1] 30097
[1] 1980
[1] 30097
[1] 30097
[1] 30097
[1] 30097
[1] 30097
[1] 49666
[1] 30097
[1] 30097
[1] 2818
[1] 1210
[1] 2818
[1] 2818
[1] 2818
[1] 20630
[1] 2818
[1] 2818
[1] 2818
[1] 2818
[1] 2818
[1] 10488
[1] 2818
[1] 2818
[1] 2818
[1] 2818
[1] 2818
[1] 37871
[1] 2818
[1] 2818
[1] 46493
[1] 42213
[1] 46493
[1] 46493
[1] 46493
[1] 23259
[1] 46493
[1] 46493
[1] 46493
[1] 46493
[1] 46493
[1] 16229
[1] 46493
[1] 46493
[1] 46493
[1] 46493
[1] 46493
[1] 42560
[1] 46493
[1] 46493
[1] 43210
[1] 24610
[1] 43210
[1] 43210
[1] 43210
[1] 34835
[1] 43210
[1] 43210
[1] 43210
[1] 43210
[1] 43210
[1] 35462
[1] 43210
[1] 43210
[1] 43210
[1] 43210
[1] 43210
[1] 45571
[1] 43210
[1] 43210
[1] 39343
[1] 47442
[1] 39343
[1] 39343
[1] 39343
[1] 21955
[1] 39343
[1] 39343
[1] 39343
[1] 39343
[1] 39343
[1] 23184
[1] 39343
[1] 39343
[1] 39343
[1] 39343
[1] 39343
[1] 9868
[1] 39343
[1] 39343
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8181/7939315488_db80a96e94_o.png) 



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

![plot of chunk fig2](http://farm9.staticflickr.com/8440/7939315694_d6872e7d42_o.png) 







