

## Chance transition



```r
load("data/zoom.rda")
dat <- subset(zoom, reps == 4)
curve(((x - 250)/300)^4 - 1.55 * ((x - 250)/300)^2, 0, 650, lwd = 2, 
    xlab = "state", ylab = "potential")
par(new = TRUE)
plot(dat$value, dat$time, type = "l", xlim = c(0, 650), xaxt = "n", 
    yaxt = "n", ylim = c(0.997 * min(dat$time), max(dat$time) * 1.005), xlab = "", 
    ylab = "")
```

![plot of chunk unnamed-chunk-1](http://farm8.staticflickr.com/7228/7296908840_76dae3c89f_o.png) 



## bifurcation



```r
curve(x^4 - 3.5 * x^2, -2, 2, lwd = 2, lty = 2, xlab = "state", ylab = "potential", 
    col = rgb(0.4, 0.4, 0.4))
curve(0.3 * (x^4 + x^3 - 3.5 * x^2), -2, 2, lwd = 2, add = T, col = rgb(0.2, 
    0.2, 0.2))
curve(0.6 * (x^4 + 0.5 * x^3 - 3.5 * x^2), -2, 2, lwd = 2, add = T)
points(1.1, 0, pch = 19, col = "darkgray", cex = 7)
```

![plot of chunk unnamed-chunk-2](http://farm8.staticflickr.com/7215/7296910046_5d8ac0a0e4_o.png) 




