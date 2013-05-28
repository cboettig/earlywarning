

```r
library(data.table)
```

```
## data.table 1.8.8 For help type: help("data.table")
```

```r
library(reshape2)
library(ggplot2)
allee <- read.csv("allee_data.csv")[2:3]
allee_null <- read.csv("allee_nulldata.csv")[2:3]
ou <- read.csv("ou_data.csv")[2:3]
ou_null <- read.csv("ou_nulldata.csv")[2:3]

df <- melt(list(OU = list(conditional = ou, null = ou_null), Allee = list(conditional = allee, 
    null = allee_null)), id = c("variable", "value"))
names(df) = c("variable", "value", "data", "model")
write.csv(df, "Figure1.csv")
df <- read.csv("Figure1.csv")
```









```r
library(grid)
library(earlywarning)
```



Some reorganization of the data...


```r
roc_me <- function(dat, nulldat) {
    test_variance = subset(dat, variable == "Variance")
    null_variance = subset(nulldat, variable == "Variance")
    test_autocorrelation = subset(dat, variable == "Autocorrelation")
    null_autocorrelation = subset(nulldat, variable == "Autocorrelation")
    
    roc_var <- roc_data(NULL, null = null_variance$value, test = test_variance$value)
    roc_acor <- roc_data(NULL, null = null_autocorrelation$value, test = test_autocorrelation$value)
    rocs <- melt(list(variance = roc_var, autocorrelation = roc_acor), id = c("Threshold", 
        "True.positives", "False.positives"))
    names(rocs)[4] = "Indicator"
    rocs
}

allee_roc <- roc_me(allee, allee_null)
ou_roc <- roc_me(ou, ou_null)
```



```r
ggplot(allee_roc) + geom_line(aes(False.positives, True.positives, col = Indicator)) + 
    theme_bw()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
ggplot(ou_roc) + geom_line(aes(False.positives, True.positives, col = Indicator)) + 
    theme_bw()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


