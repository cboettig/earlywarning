
```r
rm(list = ls())
library(earlywarning)
library(reshape2)  # data manipulation
library(data.table)  # data manipulation
library(ggplot2)  # graphics
opts_chunk$set(warning = FALSE, message = FALSE, comment = NA, tidy = FALSE, 
    verbose = TRUE, cache = FALSE, fig.path = "beer_")
```




### Conditional distribution




```r
threshold <- -4
require(sde)
M <- 2000   # replicates
N <- 20000 # sample points
d <- expression(-5 * x)
s <- expression(3.5)
X <- sde.sim(X0=0, drift=d, T=10, sigma=s, N = N, M = M)
```

```
Error: could not find function "sde.sim"
```

```r
timeseries <- matrix(X@.Data, ncol=M)
```

```
Error: object 'X' not found
```

```r
# Get the ids of samples that have a point less than the theshold
w <- sapply(data.frame(timeseries), function(x) any(x < threshold))
```

```
Error: object 'timeseries' not found
```

```r
# extract that subset by id 
W <- timeseries[,w]
```

```
Error: object 'timeseries' not found
```

```r
sample <- 1500 # sample length
dev <- sapply(as.data.frame(W), which.min)
```

```
Error: object 'W' not found
```

```r
drop <- which(dev - sample < 1)
```

```
Error: object 'dev' not found
```

```r
if(length(drop) > 0){
W <- W[,-drop]
dev <- dev[-drop]
}
```

```
Error: object 'W' not found
```

```r
D <- rbind((dev - sample), dev)
```

```
Error: object 'dev' not found
```

```r
# extract just that range
M <- as.matrix(W)
```

```
Error: object 'W' not found
```

```r
dat <- sapply(1:length(dev), function(i) M[D[1,i]:D[2,i], i])
```

```
Error: object 'dev' not found
```

```r
dat <- as.data.frame(dat)
```

```
Error: object 'dat' not found
```

```r
dat <- as.data.frame(cbind(time = 1:dim(dat)[1], dat))
```

```
Error: object 'dat' not found
```

```r
df <- melt(dat, id="time")
```

```
Error: object 'dat' not found
```

```r
names(df) = c("time", "reps", "value")
```

```
Error: names() applied to a non-vector
```

```r
levels(df$reps) <- 1:length(levels(df$reps)) # use numbers for reps instead of V1, V2, etc
```

```
Error: object of type 'closure' is not subsettable
```

```r
zoom <- df
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

```
Error: object 'reps' not found
```


Compute model-based warning signals on all each of these.  


```r
dt <- data.table(zoom)
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
```

```
Error: object 'reps' not found
```

```r
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
```

```
Error: object 'reps' not found
```

```r
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))
```

```
Error: object 'acor' not found
```


### Null distribution 

To compare against the expected distribution of these statistics, we create another set of simulations without conditioning on having experienced a chance transition, on which we perform the identical analysis.  


```r
null <- timeseries[1000:1201,]
```

```
Error: object 'timeseries' not found
```

```r
null <- as.data.frame(cbind(time = 1:dim(null)[1], null))
```

```
Error: object 'null' not found
```

```r
ndf <- melt(null, id="time")
```

```
Error: object 'null' not found
```

```r
names(ndf) = c("time", "reps", "value")
```

```
Error: object 'ndf' not found
```

```r
levels(ndf$reps) <- 1:length(levels(ndf$reps)) # use numbers for reps instead of V1, V2, etc
```

```
Error: object 'ndf' not found
```

```r
nulldt <- data.table(ndf)
```

```
Error: object 'ndf' not found
```

```r
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
```

```
Error: object 'nulldt' not found
```

```r
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
```

```
Error: object 'nulldt' not found
```

```r
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```

```
Error: object 'nullvar' not found
```



```r
ggplot(dat) + geom_histogram(aes(value, y=..density..), binwidth=0.3, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), adjust=2) + xlab("Kendall's tau") + theme_bw()
```

```
Error: object 'dat' not found
```







