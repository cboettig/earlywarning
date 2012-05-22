





In the case of the simulated model, we can compare our process of simulating under the approximate models, OU and LSN, to what we would have gotten if we could simulate under the true process.  




```
Loading required package: earlywarning
```




Now we create the replicates by simulating under the original model.  We will not try to estimate the **nonlinear** model first, as we generally do not have the power to do this well (but see [my notes](http://www.carlboettiger.info/archives/461) attempting this anyway).  




```r
require(populationdynamics)
```



```
Loading required package: populationdynamics
```



```r
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, 
    h = 200, i = 0, Da = 0.09, Dt = 0, p = 2)
time = seq(0, 500, length = 500)
sn <- saddle_node_ibm(pars, time, reps = nreps)
X <- data.frame(time = time, value = sn$x1)
```




We tidy up the data a bit


```r
require(reshape2)
```



```
Loading required package: reshape2
```



```r
dat <- melt(sn$x1)
names(dat) = c("time", "rep", "population")
```





We can set up a parallel envirnoment for a multicore machine,


```r
require(doMC)
```



```
Loading required package: doMC
```



```
Loading required package: foreach
```



```
Loading required package: iterators
```



```
Loading required package: codetools
```



```
Loading required package: multicore
```



```r
registerDoMC()
```




We can now fit the LSN model to each of these replicates, 


```r
require(plyr)
```



```
Loading required package: plyr
```



```r
models <- dlply(dat, "rep", function(X) {
    Y <- data.frame(X$time, X$population)
    stability_model(Y, "LSN")
}, .parallel = TRUE)
```




and look at the distribution of parameters.  



```r
p <- sapply(models, `[[`, "pars")
```





We compare this to simulating replicates from the **approximate** model and restimating parameters.  We will use the model average parameters as the parameters for a model that will generate our simulations.  







