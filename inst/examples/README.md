

```r
library(ggplot2)
dat <- read.csv("Figure3_dat.csv")
nulldat <- read.csv("Figure3_nulldat.csv")
ggplot(dat) + geom_histogram(aes(value, y = ..density..), binwidth = 0.3, 
    alpha = 0.5) + facet_wrap(~variable) + xlim(c(-1, 1)) + geom_density(data = nulldat, 
    aes(value), adjust = 3) + xlab("Kendall's tau") + theme_bw()
```

![plot of chunk Figure3](figure/Figure3.png) 



