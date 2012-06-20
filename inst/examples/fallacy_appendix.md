

# Code for Prosecutors Fallacy 




This code is written in the `R` language for statistical computing.  
Population dynamics are simulated using the `populationdynamics` package
(Boettiger, 2012) for exact simulations of 
discrete birth-death processes in continuous time using the Gillespie
algorithm (Gillespie, 1977).  Early warning signals
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

We begin by loading the required libraries.  

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




To take advantage of parallelization, we loop over this function a set
number of times.  The `snowfall` library provides the parallelization
of the `lapply` loop.  We initialize a parallel framework across 12
processors,

```r
sfInit(parallel=TRUE, cpu=12)
sfLibrary(populationdynamics)
sfExportAll()
```

and then loop over 20 instances of 1000 replicates each to assemble our
dataset.  A few extra commands format the data into a table with columns
of times, replicate id number, and population value at the given time.


```r
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



Finally we compute model-based warning signals on all each of these.  
We take advantage of the `data.table` package to quickly apply 
the `warningtrend` function over each of the replicates.  


```r
dt <- data.table(subset(zoom, value>250))
var <- dt[, warningtrend(
            data.frame(time=time, value=value), window_var),
            by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value),
             window_autocorr),
             by=reps]$V1
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
sfExportAll()
examples <-  sfLapply(1:24, function(i) select_crashes(50000))
nulldat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
nulldat <- melt(examples)
names(nulldat) = c("time", "reps", "value")
levels(nulldat$reps) <- 1:length(levels(dat$reps)) 
```

```r
require(plyr)
nullzoom <- ddply(nulldat, "reps", function(X){
    data.frame(time=X$time, value=X$value)
    })
```


```r
nulldt <- data.table(nullzoom)
nullvar <- nulldt[, warningtrend(
                    data.frame(time=time, value=value), window_var),
                    by=reps]$V1
nullacor <- nulldt[, warningtrend(
                     data.frame(time=time, value=value), window_autocorr),
                     by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```


## Plot the distributions 

We generate the plot of the null distribution as a density curve overlaid on the histogram of
warning signal statistics we calculated for the conditionally selected examples.  

```r
ggplot(dat) + 
 geom_histogram(aes(value, y=..density..), binwidth=0.2, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), bw=0.2)
```

![Figure2](appendix/figure2.png) 


## References

<p>Boettiger C (2012).
<EM>populationdynamics: Tools to simulate various population dynamics models in ecology</EM>.
R package version 0.0-1.

<p>Boettiger C (2012).
<EM>earlywarning: Detection of Early Warning Signals for Catastrophic Bifurcations</EM>.
R package version 0.0-1.
Boettiger C (2012). _populationdynamics: Tools to simulate various
population dynamics models in ecology_. R package version 0.0-1.

<p>Gillespie DT (1977).
&ldquo;Exact Stochastic Simulation of Coupled Chemical Reactions.&rdquo;
<EM>The Journal of Physical Chemistry</EM>, <B>81</B>.
ISSN 0022-3654, <a href="http://dx.doi.org/10.1021/j100540a008">http://dx.doi.org/10.1021/j100540a008</a>.

<p>Boettiger C and Hastings A (2012).
&ldquo;Quantifying Limits to Detection of Early Warning For Critical Transitions.&rdquo;
<EM>Journal of The Royal Society Interface</EM>.
ISSN 1742-5689, <a href="http://dx.doi.org/10.1098/rsif.2012.0125">http://dx.doi.org/10.1098/rsif.2012.0125</a>.



