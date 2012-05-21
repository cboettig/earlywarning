# Simulation example





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
data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")
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
save(list = ls(), file = "simulation.rda")
stopCluster(cl)
```



```r
require(ggplot2)
ggplot(lr) + geom_density(aes(value, fill = simulation), alpha = 0.6) + 
    geom_vline(aes(xintercept = observed))
```

![plot of chunk unnamed-chunk-5](http://farm8.staticflickr.com/7220/7244790570_1c9ff0f468_o.png) 




```r
ggplot(roc) + geom_line(aes(False.positives, True.positives))
```

![plot of chunk unnamed-chunk-6](http://farm6.staticflickr.com/5235/7244790984_b731b6ac08_o.png) 



