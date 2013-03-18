
```r
# opts_knit$set(upload.fun = socialR::flickr.url)

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
theme_publish <- theme_set(theme_bw(12))
theme_publish <- theme_update(axis.line = theme_blank(), axis.text.y = theme_blank(), 
    legend.position = c(0.18, 0.78), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
    plot.background = theme_blank(), legend.title = theme_blank())
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
```

```
## 'theme_blank' is deprecated. Use 'element_blank' instead. (Deprecated;
## last used in version 0.9.1)
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
x <- seq(lower, upper, length.out = 100)
bx <- sapply(x, b)
dx <- sapply(x, d)
d0 <- melt(data.frame(State = x, birth = bx, death = dx), id = "State")
p0 <- ggplot(d0) + geom_line(aes(State, value, lty = variable)) + 
    ylab("growth rate") + opts(title = "(a) Birth rate and death rate functions \n used in the model")
```

```
## 'opts' is deprecated. Use 'theme' instead. (Deprecated; last used in
## version 0.9.1)
```

```
## Setting the plot title with opts(title="...") is deprecated.  Use
## labs(title="...") or ggtitle("...") instead. (Deprecated; last used in
## version 0.9.1)
```

```r
write.csv(d0, "Figure1_panela.csv")
```



```r
a <- 180
u1 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
a <- 190
u2 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
a <- 200
u3 <- -1 * sapply(x, function(x) integrate(f, lower, x)$value)
```




```r
load("data/zoom.rda")
dat <- subset(zoom, reps == 2)
# dat <- subset(dat, value > 250)
a <- dat$time - min(dat$time)
a <- a/max(a) * (34000 - 19000) + 19000
d2 <- data.frame(x = dat$value, value = a)
```



```r
df <- data.frame(x=x, value=u1)
df$variable = "stable potential"
d2$variable = "stable potential"
df$panel <- "Potential Energy"
d2$panel <- "Time of Sample Trajectory"
d <- rbind(df, d2)
p1 <- ggplot(data = d, mapping = aes(x = x, y = value)) + 
  facet_grid(panel ~ ., scale = "free") +
  layer(data = d2, geom = "point", size=1) +
  layer(data = df,  mapping = aes(lty=variable),  geom = c("line")) +
  geom_vline(aes(xintercept=250), lty=4) +
  coord_cartesian(ylim=c(18000, 35000)) + 
  xlab("State") + ylab("arbitrary units") +
  opts(title = "(b) Trajectory of a chance \n transition from a stable system")
```

```
## 'opts' is deprecated. Use 'theme' instead. (Deprecated; last used in
## version 0.9.1)
```

```
## Setting the plot title with opts(title="...") is deprecated.  Use
## labs(title="...") or ggtitle("...") instead. (Deprecated; last used in
## version 0.9.1)
```

```r

write.csv(d, "Figure1_panelb.csv")
```






simulate an example with a bifurcation for comparison.  


```r
library(populationdynamics)
T <- 5000
n_pts <- 50000
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200, i = 0, Da = 0.5, 
    Dt = 0, p = 2)
sn <- saddle_node_ibm(pars, times = seq(0, T, length = n_pts), reps = 1)
X <- sn$x1
X <- X[X > 0]
times <- 1:length(X) * T/n_pts
a <- times - min(times)
a <- a/max(a) * (34000 - 19000) + 19000
d3 <- data.frame(x = X, value = a)
```





```r
df <- data.frame(x=x, "stable potential"=u1, "deteriorating"=u2, "critical"=u3)
d1 <- melt(df, id="x")
d1$panel <- "Potential Energy"
d3$panel <- "Time of Sample Trajectory"
d3$variable <- d3$x # match col numbers to rbind
d <- rbind(d1, d3)
```

```
## Warning: invalid factor level, NAs generated
```

```r
p2 <- ggplot(data = d, mapping = aes(x = x, y = value)) + 
  facet_grid(panel ~ ., scale = "free") +
  layer(data = d3, geom = "point", size=1) +
  layer(data = d1,  mapping = aes(lty=variable), geom = c("line")) +
  geom_vline(aes(xintercept=250), lty=4) +
  coord_cartesian(ylim=c(18000, 35000)) + xlab("State") + ylab("arbitrary units") +
  opts(title="(c) Trajectory during an \n underlying parameter shift")
```

```
## 'opts' is deprecated. Use 'theme' instead. (Deprecated; last used in
## version 0.9.1)
```

```
## Setting the plot title with opts(title="...") is deprecated.  Use
## labs(title="...") or ggtitle("...") instead. (Deprecated; last used in
## version 0.9.1)
```

```r
write.csv(d, "Figure1_panelc.csv")
```



```r
require(grid)
```

```
## Loading required package: grid
```

```r
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p0, vp = vplayout(1, 1))
print(p1, vp = vplayout(1, 2))
print(p2, vp = vplayout(1, 3))
```

![plot of chunk Figure1_upload](figure/Figure1_upload.png) 



```r
require(grid)
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p0, vp = vplayout(1, 1))
print(p1, vp = vplayout(1, 2))
print(p2, vp = vplayout(1, 3))
```

![plot of chunk Figure1_orig](figure/Figure1_orig.pdf) 


