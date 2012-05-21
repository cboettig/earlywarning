
# Glaciation data example





```r
library(earlywarning)
library(snow)
library(methods)
cl <- makeCluster(20, type = "MPI")
```



```
## 	20 slaves are spawned successfully. 0 failed.
```



```r
clusterEvalQ(cl, library(earlywarning))
```





```r
data("glaciation")
A <- stability_model(glaciation, "OU")
B <- stability_model(glaciation, "LSN")
observed <- -2 * (logLik(A) - logLik(B))
```






```r
clusterExport(cl, ls())
clusterExport(cl, list = c("A", "B"))
reps <- parLapply(cl, 1:500, function(i) compare(A, B))
lr <- lik_ratios(reps)
roc <- roc_data(lr)
```






```r
save(list = ls(), file = "glaciation.rda")
stopCluster(cl)
```







```r
require(ggplot2)
ggplot(lr) + geom_density(aes(value, fill = simulation), alpha = 0.6) + 
    geom_vline(aes(xintercept = observed))
```

![plot of chunk unnamed-chunk-5](http://farm8.staticflickr.com/7238/7245474810_ef1bc88c7f_o.png) 




```r
ggplot(roc) + geom_line(aes(False.positives, True.positives))
```

![plot of chunk unnamed-chunk-6](http://farm8.staticflickr.com/7229/7245475144_da26bcf9c6_o.png) 



