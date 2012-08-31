



```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(reshape2)
```

```
## Loading required package: reshape2
```



```r
e <- 0.5
a <- 180
K <- 1000
h <- 200
b <- function(n) (0.5 * K * n^2)/(n^2 + h^2)
d <- function(n) e * n + a
f <- function(n) b(n) - d(n)
```





```r
lower <- 0
upper <- 700
curve(b, lower, upper, lwd = 2, xlab = "state", ylab = "growth rate")
curve(d, lower, upper, lwd = 2, add = T, col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 











```r

x <- seq(lower, upper, length.out = 100)
a <- 180
u1 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
a <- 190
u2 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
a <- 200
u3 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
```





```r
load("data/zoom.rda")
dat <- subset(zoom, reps == 1)
dat <- subset(dat, value > 250)
a <- dat$time - min(dat$time)
a <- a/max(a) * (34000 - 19000) + 19000
d2 <- data.frame(x = dat$value, value = a)
```




```r
plot(x, u3, type = "l", lwd = 1, lty = 2, xlab = "state", ylab = "potential")
lines(x, u1, lwd = 2, xlab = "state", ylab = "potential")
lines(x, u2, lwd = 1, lty = 2, xlab = "state", ylab = "potential")
par(new = TRUE)
plot(dat$value, dat$time, type = "l", xlim = c(lower, upper), xaxt = "n", 
    yaxt = "n", ylim = c(0.997 * min(dat$time), max(dat$time) * 1.005), xlab = "", 
    ylab = "")
axis(4)
mtext("time", 4)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


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
a <- times - min(times)
a <- a/max(a) * (34000 - 19000) + 19000
d3 <- data.frame(x = X, value = a)
```



```r
df <- data.frame(x=x, value=u1)
df$panel <- "Potential Energy"
d2$panel <- "Time of Sample Trajectory"
d <- rbind(df, d2)
ggplot(data = d, mapping = aes(x = x, y = value)) + 
  facet_grid(panel ~ ., scale = "free") +
  layer(data = d2, geom = "point") +
  layer(data = df,  geom = c("line")) +
  coord_cartesian(ylim=c(18000, 35000)) + xlab("State") + 
  theme_bw() + opts(axis.line=theme_blank(),
        axis.text.y=theme_blank(),
        axis.title.y=theme_blank(),legend.position="none",
        panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),
        plot.background=theme_blank())
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 






```r
df <- data.frame(x = x, `stable potential` = u1, deteriorating = u2, 
    critical = u3)
d1 <- melt(df, id = "x")
d1$panel <- "Potential Energy"
d3$panel <- "Time of Sample Trajectory"
d3$variable <- d3$x  # match col numbers to rbind
d <- rbind(d1, d3)
```

```
## Warning: invalid factor level, NAs generated
```

```r
ggplot(data = d, mapping = aes(x = x, y = value)) + facet_grid(panel ~ 
    ., scale = "free") + layer(data = d3, geom = "point") + layer(data = d1, 
    mapping = aes(lty = variable), geom = c("line")) + coord_cartesian(ylim = c(18000, 
    35000)) + xlab("State") + theme_bw() + opts(axis.line = theme_blank(), axis.text.y = theme_blank(), 
    axis.title.y = theme_blank(), legend.position = "none", panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), plot.background = theme_blank())
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 





