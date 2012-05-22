

Load libraries and set up the parallel processing environment for 20 clusters



```r
require(earlywarning)
require(snow)
require(ggplot2)
require(reshape2)
cl <- makeCluster(20, type = "MPI")
```



```
## 	20 slaves are spawned successfully. 0 failed.
```



```r
clusterEvalQ(cl, library(earlywarning))
```



```
## [[1]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[2]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[3]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[4]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[5]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[6]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[7]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[8]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[9]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[10]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[11]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[12]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[13]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[14]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[15]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[16]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[17]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[18]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[19]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
## [[20]]
##  [1] "earlywarning" "snow"         "Rmpi"         "methods"     
##  [5] "stats"        "graphics"     "grDevices"    "utils"       
##  [9] "datasets"     "base"        
## 
```




Fit both models to the original data, record the observed likelihood ratio of the original data



```r
data("chemostat")
A.chemo <- stability_model(chemostat, "OU")
B.chemo <- stability_model(chemostat, "LSN")
observed.chemo <- -2 * (logLik(A.chemo) - logLik(B.chemo))
```





Perform the bootstrapped model comparison using 500 replicates on the parallel cluster.  



```r
clusterExport(cl, ls())
clusterExport(cl, list = c("A.chemo", "B.chemo"))
reps.chemo <- parLapply(cl, 1:500, function(i) compare(A.chemo, B.chemo))
lr.chemo <- lik_ratios(reps.chemo)
roc.chemo <- roc_data(lr.chemo)
```






```r
require(ggplot2)
ggplot(lr.chemo) + geom_density(aes(value, fill = simulation), alpha = 0.6) + 
    geom_vline(aes(xintercept = observed.chemo))
```

![plot of chunk lr_ratio](figure/lr_ratio.png) 




```r
ggplot(roc.chemo) + geom_line(aes(False.positives, True.positives))
```

![plot of chunk roc](figure/roc.png) 






```r
X <- chemostat
pow <- reps.chemo
```




Plot of the raw data



```r
rawdata <- data.frame(time = as.numeric(time(X)), state = X@.Data)
p_raw <- ggplot(rawdata, aes(time, state)) + geom_line() + opts(title = paste(pow$label, 
    "timeseries data"))
p_raw
```

![plot of chunk rawdata](figure/rawdata.png) 




```r
save(list = ls(), file = "chemostat.rda")
```



