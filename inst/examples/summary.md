



```r
library(data.table)
```



```
data.table 1.8.0  For help type: help("data.table")
```



```r
library(reshape2)
library(earlywarning)
library(ggplot2)
load("success.rda")
dt <- data.table(subset(zoom, value > 250))
var <- dt[, warningtrend(data.frame(time = time, value = value), 
    window_var), by = reps]$V1
acor <- dt[, warningtrend(data.frame(time = time, value = value), 
    window_autocorr), by = reps]$V1
dat <- melt(data.frame(var = var, acor = acor))
```



```
Using  as id variables
```



```r
ggplot(dat) + geom_histogram(aes(value), binwidth = 0.2) + facet_wrap(~variable) + 
    xlim(c(-1, 1))
```

![plot of chunk unnamed-chunk-1](http://farm9.staticflickr.com/8166/7184272034_a7a25808ef_o.png) 



