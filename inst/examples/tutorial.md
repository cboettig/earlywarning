
We will need the following libraries to run this example.  If you don't have them, they can be installed from CRAN using the `install.packages` function.  


```
## Loading required package: bibtex
```




```r
library(ggplot2)
library(reshape2)
library(multicore)
library(devtools)
```


We will be using custom functions provided in the R package Boettiger, (2012) that accompanies Boettiger & Hastings, (2012), which can be installed directly from the development repository on GitHub:


```r
install_github("earlywarning", "cboettig")
library(earlywarning)
```




We begin by loading the chemostat data provided by Drake & Griffen, (2010). We plot the raw data to take a look at it. 



```r
data("chemostat")
plot(chemostat, type="b")
```

![plot of chunk unnamed-chunk-1](http://farm9.staticflickr.com/8241/8641760994_bc3d59ee14_o.png) 



Fit both models to the original data, record the observed likelihood ratio of the original data


```r
A <- stability_model(chemostat, "OU")
B <- stability_model(chemostat, "LSN")
observed <- -2 * (logLik(A) - logLik(B))
```



Perform the bootstrapped model comparison on the parallel cluster.  


```r
runtime <- system.time(
reps <- mclapply(1:2000, function(i) compare(A, B)))
```


Which took 1.2369 &times; 10<sup>4</sup> seconds to run for the example shown here.  

A helper function extracts the likelihood ratios under each of the pairwise comparisons (The null distribution: -2 times the log likelihood of data fit under A that had been simulated under A, minus the log likelihood of fits under A  simulated under B; and the test distribution: fit under B when simulated under A, minus the loglikehood of being fit under B, simulated under B) as a data frame.  we show the top of the data frame as output below.  



```r
lr <- lik_ratios(reps)
head(lr)
```

```
##   simulation rep     value
## 1       null   1  0.004580
## 2       test   1  0.002791
## 3       null   2  0.031427
## 4       test   2  1.006297
## 5       null   3  0.003951
## 6       test   3 -0.055753
```


We use this data to generate the overlapping distributions shown in Boettiger & Hastings, (2012), 


```r
ggplot(lr) + geom_density(aes(value, fill=simulation), alpha=0.6) + geom_vline(aes(xintercept=observed))
```

![plot of chunk lr_ratio_plot](http://farm9.staticflickr.com/8384/8641016997_75a7d46e80_o.png) 



A second helper function generates the ROC curve from the likelihood ratio data, providing an alternative way to visualize this overlap:


```r
roc <- roc_data(lr)
ggplot(roc) + geom_line(aes(False.positives, True.positives))
```

![plot of chunk roc](http://farm9.staticflickr.com/8543/8641017081_313e85c6a4_o.png) 






ROC curves are particularly useful in comparing the power of a variety of approaches.  For instance, we can compare the performance of the likelihood-based signal shown above to the traditional approach of using a correlation coefficient to detect the increase in some summary statistic such as variance or autocorrelation.  To obtain replicates, we will still simulate under the null (OU) and test (LSN) models estimated above, but instead of computing the likelihood of these data, we will estimate the commonly used rank correlation coefficient, Kendall's tau, to quantify the increase in variance, autcorrelation, and skew observed in a moving window over the data.



```r
var <- bootstrap_trend(chemostat, window_var, method="kendall", rep=2000)
acor <- bootstrap_trend(chemostat, window_autocorr, method="kendall", rep=2000)
skew <- bootstrap_trend(chemostat, window_skew, method="kendall", rep=2000)
```


These data are formatted like the likeihood ratio data above, only that the statistic is now Kendall's tau.  



```r
ggplot(var) + geom_density(aes(value, fill=simulation), alpha=0.6) + geom_vline(aes(xintercept=observed))
```

![plot of chunk var_ratio](http://farm9.staticflickr.com/8248/8641020179_b81e148185_o.png) 



We can combine the data for the ROC curves of each indicator to compare:


```r
indicators <- melt(list(var = roc_data(var), acor = roc_data(acor), skew = roc_data(skew), lr = roc), id = c("Threshold", "False.positives", "True.positives"))
ggplot(indicators) + geom_line(aes(False.positives, True.positives, color=L1)) 
```

![plot of chunk unnamed-chunk-3](http://farm9.staticflickr.com/8257/8641020295_4e6889d809_o.png) 


