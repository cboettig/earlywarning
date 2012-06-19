

# Code for Prosecutors Fallacy 
`TRUE`
This code is written in the `R` language for statistical computing.  
Population dynamics are simulated using the `populationdynamics` package
(Boettiger, 2012) for exact simulations of 
discrete birth-death processes in continuous time using the Gillespie
agorithm (Gillespie, 1977).  Early warning signals
of variance and autocorrelation, as well as the model-based estimate
of Boettiger & Hastings, (2012) are estimated using the 
`earlywarning` package (Boettiger, 2012).  These
packages can be installed from Github using the `devtools` R package

```r
library(devtools)
install_github("populationdynamics", "cboettig")
install_github("earlywarning", "cboettig")
```

In the examples of this manuscript, the population dynamics are given by

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




### Conditional distribution

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 50,000 pts a piece, we can save memory by performing the conditional selection on the ones that crash as we go along and disgard the others.  (We will create a null distribution in which we ignore this conditional selection later).  




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




To take advantage of parallelization, we loop over this function a set number of times.  The `snowfall` library provides the parallelization
of the `lapply` loop.  A few extra commands format the data into a table
with columns of times, replicate id number, and population value at the
given time.




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
```






Compute model-based warning signals on all each of these.  



```r
dt <- data.table(subset(zoom, value>250))
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))
```




### Null distribution 

To compare against the expected distribution of these statistics, we create another set of simulations without conditioning on having experienced a chance transition, on which we perform the identical analysis.  



```r
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
sfInit(parallel=TRUE, cpu=12)
sfLibrary(populationdynamics)
```



```
Library populationdynamics loaded.
```



```r
sfExportAll()
examples <-  sfLapply(1:24, function(i) select_crashes(50000))
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
ggplot(dat) + geom_histogram(aes(value, y=..density..), binwidth=0.2, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), bw=0.2)
```

![plot of chunk figure2](appendix/figure2.png) 


list(title = "populationdynamics: Tools to simulate various population dynamics models in ecology", author = list(list(given = "Carl", family = "Boettiger", role = NULL, email = "cboettig@gmail.com", comment = NULL)), year = "2012", note = "R package version 0.0-1"), list(title = "Exact Stochastic Simulation of Coupled Chemical Reactions", author = list(list(given = c("Daniel", "T."), family = "Gillespie", role = NULL, email = NULL, comment = NULL)), journal = "The Journal of Physical Chemistry", year = "1977", month = "12", volume = "81", doi = "10.1021/j100540a008", issn = "0022-3654"), list(title = "Quantifying Limits to Detection of Early Warning For Critical Transitions", author = list(list(given = "C.", family = "Boettiger", role = NULL, email = NULL, comment = NULL), list(given = "A.", family = "Hastings", role = NULL, email = NULL, comment = NULL)), journal = "Journal of The Royal Society Interface", year = "2012", month = "05", doi = "10.1098/rsif.2012.0125", issn = "1742-5689")
