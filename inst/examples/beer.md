

 

```r
rm(list=ls())
library(earlywarning)
library(reshape2)		# data manipulation
library(data.table)	# data manipulation
library(ggplot2)		# graphics
```




### Conditional distribution




```r
threshold <- -2.5
require(sde)
```

```

To check the errata corrige of the book, type vignette("sde.errata")
```

```r
M <- 2000   # replicates
N <- 20000 # sample points
d <- expression(-1 * x)
s <- expression(1)
X <- sde.sim(X0=0, drift=d, T=5, sigma=s, N = N, M = M)
```

```
sigma.x not provided, attempting symbolic derivation.
```

```r
timeseries <- matrix(X@.Data, ncol=M)
# Get the ids of samples that have a point less than the theshold
w <- sapply(data.frame(timeseries), function(x) any(x < threshold))
# extract that subset by id 
W <- timeseries[,w]
sample <- 1500 # sample length
dev <- sapply(as.data.frame(W), which.min)
drop <- which(dev - sample < 1)
if(length(drop) > 0){
W <- W[,-drop]
dev <- dev[-drop]
}
D <- rbind((dev - sample), dev)
# extract just that range
M <- as.matrix(W)
dat <- sapply(1:length(dev), function(i) M[D[1,i]:D[2,i], i])
dat <- as.data.frame(dat)
dat <- as.data.frame(cbind(time = 1:dim(dat)[1], dat))
df <- melt(dat, id="time")
names(df) = c("time", "reps", "value")
levels(df$reps) <- 1:length(levels(df$reps)) # use numbers for reps instead of V1, V2, etc
zoom <- df
```




```r
write.csv(zoom, file="trajectories.csv")
```



```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8533/8673040130_00d027678f_o.png) 


Compute model-based warning signals on all each of these.  


```r
dt <- data.table(zoom)
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))
```




```r
marc <- function(x){
  y <- x[-1]
  v <- y - x[-length(x)] # step differences
  ratio <- sum(v > 0) / sum(v < 0)
  ratio
}
ratio <- dt[, marc(value), by=reps]
ggplot(ratio) + geom_histogram(aes(x=V1))
```

![plot of chunk marc_ratio](http://farm9.staticflickr.com/8521/8671938917_e790df65b2_o.png) 



```r
cr <- function(df) unname(cor.test(df[[1]], df[[2]], method="kendall")$estimate)
taus <- dt[, cr(data.frame(time, value)), by=reps]
ggplot(taus) + geom_histogram(aes(x=V1))
```

![plot of chunk kendall_data](http://farm9.staticflickr.com/8542/8673040310_01930e5744_o.png) 



### Null distribution 

To compare against the expected distribution of these statistics, we create another set of simulations without conditioning on having experienced a chance transition, on which we perform the identical analysis.  


```r
null <- timeseries[1000:2501,]
null <- as.data.frame(cbind(time = 1:dim(null)[1], null))
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

![plot of chunk fig](http://farm9.staticflickr.com/8113/8673040378_25f25e3dcc_o.png) 








```r
write.csv(dat, file="beer_dat.csv")
write.csv(nulldat, file="beer_nulldat.csv")
```

