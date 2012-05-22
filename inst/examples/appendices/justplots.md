



```r
library(ggplot2)
pdf.options(pointsize=8)
theme_bw()
```



```
$axis.line
theme_blank()

$axis.text.x
theme_text(family = base_family, size = base_size * 0.8, vjust = 1, 
    lineheight = 0.9)

$axis.text.y
theme_text(family = base_family, size = base_size * 0.8, hjust = 1, 
    lineheight = 0.9)

$axis.ticks
theme_segment(colour = "black", size = 0.2)

$axis.title.x
theme_text(family = base_family, size = base_size, vjust = 0.5)

$axis.title.y
theme_text(family = base_family, size = base_size, vjust = 0.5, 
    angle = 90)

$axis.ticks.length
[1] 0.15cm

$axis.ticks.margin
[1] 0.1cm

$legend.background
theme_rect(colour = NA)

$legend.margin
[1] 0.2cm

$legend.key
theme_rect(colour = "grey80")

$legend.key.size
[1] 1.2lines

$legend.key.height
NULL

$legend.key.width
NULL

$legend.text
theme_text(family = base_family, size = base_size * 0.8)

$legend.text.align
NULL

$legend.title
theme_text(family = base_family, face = "bold", size = base_size * 
    0.8, hjust = 0)

$legend.title.align
NULL

$legend.position
[1] "right"

$legend.direction
NULL

$legend.justification
[1] "center"

$legend.box
NULL

$panel.background
theme_rect(fill = "white", colour = NA)

$panel.border
theme_rect(fill = NA, colour = "grey50")

$panel.grid.major
theme_line(colour = "grey90", size = 0.2)

$panel.grid.minor
theme_line(colour = "grey98", size = 0.5)

$panel.margin
[1] 0.25lines

$strip.background
theme_rect(fill = "grey80", colour = "grey50")

$strip.text.x
theme_text(family = base_family, size = base_size * 0.8)

$strip.text.y
theme_text(family = base_family, size = base_size * 0.8, angle = -90)

$plot.background
theme_rect(colour = NA)

$plot.title
theme_text(family = base_family, size = base_size * 1.2)

$plot.margin
[1] 1lines   1lines   0.5lines 0.5lines

attr(,"class")
[1] "options"
```






```r
library(data.table)
library(reshape2)
library(earlywarning)
load("zoom.rda")
ggplot(subset(zoom, value>250 & reps %in% levels(zoom$reps)[1:9])) + 
  geom_line(aes(time, value)) +
  facet_wrap(~reps, scales="free") +
  theme_bw()
```

![plot of chunk replicate_crashes](./replicate_crashes.pdf) 






```r
dt <- data.table(subset(zoom, value>250))
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))

load("nullzoom3.rda")
nulldt <- data.table(nullzoom)
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```






```r
ggplot(dat) + 
  geom_histogram(aes(value, y=..density..), binwidth=0.2, alpha=.5) +
  facet_wrap(~variable) + xlim(c(-1, 1)) + 
  geom_density(data=nulldat, aes(value), bw=0.2) + theme_bw()
```

![plot of chunk indicators](./indicators.pdf) 


 


```r
#ggplot(dat) + geom_line(aes(time, value, group=reps), alpha=.01) 
```





