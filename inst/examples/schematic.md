

## Chance transition

Overplot the data of one of the replicates. 



```r
load("data/zoom.rda")
dat <- subset(zoom, reps == 4)
curve(((x - 250)/300)^4 - 1.55 * ((x - 250)/300)^2, 0, 650, lwd = 2, 
    xlab = "state", ylab = "potential")
par(new = TRUE)
plot(dat$value, dat$time, type = "l", xlim = c(0, 650), xaxt = "n", 
    yaxt = "n", ylim = c(0.997 * min(dat$time), max(dat$time) * 1.005), xlab = "", 
    ylab = "")
axis(4)
```

![plot of chunk unnamed-chunk-1](http://farm9.staticflickr.com/8151/7309161398_32053c6fb9_o.png) 



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
plot(X, times, type = "l")
```

![plot of chunk unnamed-chunk-2](http://farm8.staticflickr.com/7239/7309162124_5f6691da42_o.png) 





```r
curve(((x - 250)/300)^4 - 1.55 * ((x - 250)/300)^2, 0, 650, lwd = 2, 
    xlab = "state", ylab = "potential", col = rgb(0.4, 0.4, 0.4), lty = 2)
curve(0.5 * (((x - 250)/300)^4 + ((x - 250)/300)^3 - 1.55 * ((x - 
    250)/300)^2), lwd = 2, add = T, col = rgb(0.2, 0.2, 0.2), lty = 2)
curve(0.2 * (((x - 250)/300)^4 + 5 * ((x - 250)/300)^3 - 1.55 * ((x - 
    250)/300)^2), lwd = 2, add = T)
par(new = TRUE)
plot(times, X, type = "l", xlim = c(0, 650), xaxt = "n", yaxt = "n", 
    ylim = c(0.997 * min(dat$time), max(dat$time) * 1.005), xlab = "", ylab = "")
axis(4)
```

![plot of chunk unnamed-chunk-3](http://farm9.staticflickr.com/8152/7309162508_94af3873b3_o.png) 

```r
# points(550, 0, pch=19, col='darkgray', cex=7)
```






