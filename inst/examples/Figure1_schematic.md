`ro upload.fun=socialR::flickr.url, dev='Cairo_pdf')`


## Chance transition

Overplot the data of one of the replicates. 



```r
load("data/zoom.rda")
dat <- subset(zoom, reps == 4)
f1 <- function(x) ((x - 250)/300)^4 - 1.55 * ((x - 250)/300)^2
curve(f1, 0, 650, lwd = 2, xlab = "state", ylab = "potential")
par(new = TRUE)
plot(dat$value, dat$time, type = "l", xlim = c(0, 650), xaxt = "n", 
    yaxt = "n", ylim = c(0.997 * min(dat$time), max(dat$time) * 1.005), xlab = "", 
    ylab = "")
axis(4)
mtext("time", 4)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.pdf) 



## bifurcation

simulate an example with a bifurcation for comparison.  



```r
library(populationdynamics)
T <- 5000
n_pts <- 50000
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200, i = 0, Da = 0.5, 
    Dt = 0, p = 2)
sn <- saddle_node_ibm(pars, times = seq(0, T, length = n_pts), reps = 1)
X <- sn$x1
X <- X[X > 250]
times <- 1:length(X) * T/n_pts
```







```r
f1 <- function(x) ((x - 250)/300)^4 - 1.55 * ((x - 250)/300)^2
f2 <- function(x) 0.5 * (((x - 250)/300)^4 + ((x - 250)/300)^3 - 
    1.55 * ((x - 250)/300)^2)
f3 <- function(x) 0.2 * (((x - 250)/300)^4 + 5 * ((x - 250)/300)^3 - 
    1.55 * ((x - 250)/300)^2)
state = seq(0, 650, length = 100)
bifur <- data.frame(state, f1 = f1(state), f2 = f2(state), f3 = f3(state))
par(new = FALSE)
plot(bifur$state, bifur$f1, type = "l", lwd = 2, xlab = "state", 
    ylab = "potential", col = rgb(0.4, 0.4, 0.4), lty = 2)
lines(bifur$state, bifur$f2, lwd = 2, col = rgb(0.2, 0.2, 0.2), lty = 2)
lines(bifur$state, bifur$f3, lwd = 2)
par(new = TRUE)
plot(X, times, type = "l", axes = FALSE, xlim = c(0, 650), ylim = c(0, 
    1.2 * max(times)), xlab = "", ylab = "")
axis(4)
mtext("time", 4)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.pdf) 

```r
# points(550, 0, pch=19, col='darkgray', cex=7)
```










