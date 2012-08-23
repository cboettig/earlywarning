

# Code for Prosecutors Fallacy 
`FALSE`
This code is written in the `R` language for statistical computing.  
Population dynamics are simulated using the `populationdynamics` package


```

Error in eval(expr, envir, enclos) : could not find function "citep"

```

 for exact simulations of 
discrete birth-death processes in continuous time using the Gillespie
agorithm 

```

Error in eval(expr, envir, enclos) : could not find function "citep"

```

.  Early warning signals
of variance and autocorrelation, as well as the model-based estimate
of 

```

Error in eval(expr, envir, enclos) : could not find function "citet"

```

 are estimated using the 
`earlywarning` package 

```

Error in parse(text = code[i]) : 2:0: unexpected end of input
1: citep(citation("earlywarning")
  ^

```

.  These
packages can be installed from Github using the `devtools` R package

```r
library(devtools)
install_github("populationdynamics", "cboettig")
install_github("earlywarning", "cboettig")
```

For the individual-based simulation, the population dynamics are given by

<div>
\begin{align}
  \frac{dP(n,t)}{dt} &= b_{n-1} P(n-1,t) + d_{n+1}P(n+1,t) - (b_n+d_n) P(n,t)  \label{master}, \\
    b_n &= \frac{e K n^2}{n^2 + h^2}, \\
    d_n &= e n + a,
\end{align}
</div>

which is provided by the `saddle_node_ibm` model in `populationdynamics`. 

We also consider the discrete time version of the model of 

```

Error in eval(expr, envir, enclos) : could not find function "citet"

```

,

<div>
\begin{equation}
X_{t+1} =     X_t  \exp\left( r \left(1 - \frac{ X_t }{  K } \right) - \frac{ a * X_t ^ {Q - 1} }{s ^ Q + H ^ Q} \right) 
\end{equation}

We will use parameters r = .75, k = 10, a=1.7, H=1, Q = 3.  In this model Q is a parameter that will force the system through a bifurcation point at a = 2.  

For each of the warning signal statistics in question, 
we need to generate the distibution over all replicates
and then over replicates which have been selected conditional 
on having experienced a crash.  

We begin by running the simulation of the process for all replicates.  

Load the required libraries
 


```r
library(populationdynamics)
library(earlywarning)
library(reshape2)		# data manipulation
library(data.table)	# data manipulation
library(ggplot2)		# graphics
library(snowfall)		# parallel
```




### Conditional distribution

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 50,000 pts a piece, we can save memory by performing the conditional selection on the ones that crash as we go along and disgard the others.  (We will create a null distribution in which we ignore this conditional selection later).  




```r
select_crashes <- function(n){
	T<- 5000
	n_pts <- n
	pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
	sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1000)
	d <- dim(sn$x1)
	crashed <- which(sn$x1[d[1],]==0)
	sn$x1[,crashed] 
}
```





This setting toggles the simulation model to use the May formulation instead.  



```r
threshold <- 1.5
select_crashes <- function(n){
  n <- n/10 # doesn't need as long as the individual-based
  sn <- 
  sapply(1:1000, function(rep){
    x <- vector(mode="double", length=n)
    x[1] <- 8 # positive equilibrium
    z <- rlnorm(n, 0, .1)
    r = .75; k = 10; a=1.55; H=1; Q = 3
    for(t in 1:n){
      x[t+1] = z[t] *  x[t] * exp(r * (1 - x[t] / k) - a * x[t] ^ (Q - 1) / (x[t] ^ Q + H ^ Q)) 
    }
    x
  })
	crashed <- which(sn[n,] < threshold)
  # length(crashed)/1000 # fraction that crash
	sn[,crashed] 
}
```











To take advantage of parallelization, we loop over this function a set number of times.  The `snowfall` library provides the parallelization
of the `lapply` loop.  A few extra commands format the data into a table
with columns of times, replicate id number, and population value at the
given time.




```r
sfInit(parallel=TRUE, cpu=12)
```

```
R Version:  R version 2.15.0 (2012-03-30) 

```

```r
sfLibrary(populationdynamics)
```

```
Library populationdynamics loaded.
```

```r
sfExportAll()
examples <- sfLapply(1:20, function(i) select_crashes(50000))
dat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
names(dat) = c("time", "reps", "value")
levels(dat$reps) <- 1:length(levels(dat$reps)) # use numbers for reps
```







```r
ggplot(subset(dat, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

```
Error: object 'zoom' not found
```





Zoom in on the relevant area of data near the crash



```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value<threshold))
print(tip)
    index <- max(tip-500,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
```

```
[1] 4195
[1] 622
[1] 2224
[1] 26
[1] 4899
[1] 1216
[1] 1075
[1] 3032
[1] 116
[1] 3859
[1] 187
[1] 2807
[1] 1700
[1] 291
[1] 1586
[1] 1587
[1] 1218
[1] 2274
[1] 1303
[1] 4264
[1] 3061
[1] 2322
[1] 1299
[1] 3050
[1] 3947
[1] 2254
[1] 1830
[1] 31
[1] 42
[1] 4676
[1] 1367
[1] 3374
[1] 3654
[1] 485
[1] 2480
[1] 291
[1] 2575
[1] 3918
[1] 359
[1] 4234
[1] 3374
[1] 1221
[1] 3431
[1] 2935
[1] 3133
[1] 3051
[1] 588
[1] 2948
[1] 2110
[1] 4446
[1] 4952
[1] 3603
[1] 1615
[1] 3056
[1] 610
[1] 4430
[1] 3656
[1] 4888
[1] 3863
[1] 3924
[1] 4756
[1] 3081
[1] 790
[1] 3604
[1] 2750
[1] 4336
[1] 4857
[1] 3680
[1] 3743
[1] 3502
[1] 184
[1] 3863
[1] 4441
[1] 896
[1] 1591
[1] 4648
[1] 1042
[1] 3821
[1] 4407
[1] 194
[1] 84
[1] 1703
[1] 2608
[1] 2071
[1] 4824
[1] 2873
[1] 2872
[1] 3097
[1] 3301
[1] 495
[1] 1594
[1] 1698
[1] 4033
[1] 1759
[1] 2661
[1] 2292
[1] 396
[1] 4347
[1] 3488
[1] 3799
[1] 3344
[1] 293
[1] 3116
[1] 829
[1] 4340
[1] 939
[1] 662
[1] 1253
[1] 3035
[1] 1817
[1] 2607
[1] 4766
[1] 590
[1] 2673
[1] 3777
[1] 1766
[1] 1929
[1] 1696
[1] 1080
[1] 1347
[1] 2591
[1] 618
[1] 2106
[1] 3774
[1] 2891
[1] 3225
[1] 2487
[1] 1476
[1] 1781
[1] 2703
[1] 2712
[1] 4967
[1] 2193
[1] 3942
[1] 729
[1] 42
[1] 4970
[1] 2406
[1] 4560
[1] 1444
[1] 4819
[1] 4194
[1] 2104
[1] 1730
[1] 3953
[1] 1464
[1] 82
[1] 953
[1] 552
[1] 3242
[1] 2729
[1] 1256
[1] 305
[1] 1391
[1] 3432
[1] 751
[1] 2932
[1] 486
[1] 45
[1] 783
[1] 780
[1] 4470
[1] 689
[1] 674
[1] 4924
[1] 117
[1] 3477
[1] 2597
[1] 1411
[1] 3636
[1] 4657
[1] 762
[1] 1370
[1] 4665
[1] 385
[1] 2944
[1] 72
[1] 1671
[1] 4395
[1] 3222
[1] 1419
[1] 622
[1] 4531
[1] 2519
[1] 4591
[1] 519
[1] 178
[1] 1196
[1] 461
[1] 1552
[1] 4236
[1] 162
[1] 4509
[1] 1038
[1] 1465
[1] 309
[1] 1920
[1] 3498
[1] 3308
[1] 4586
[1] 2638
[1] 1200
[1] 1355
[1] 1364
[1] 4082
[1] 53
[1] 1185
[1] 1184
[1] 1338
[1] 41
[1] 2293
[1] 1196
[1] 1525
[1] 3016
[1] 2404
[1] 1294
[1] 371
[1] 4865
[1] 1278
[1] 3729
[1] 1674
[1] 1238
[1] 4459
[1] 1032
[1] 2262
[1] 4128
[1] 235
[1] 4301
[1] 3323
[1] 3812
[1] 169
[1] 2571
[1] 3714
[1] 3490
[1] 4171
[1] 3678
[1] 4736
[1] 4148
[1] 3953
[1] 3688
[1] 4169
[1] 2042
[1] 2336
[1] 4306
[1] 2672
[1] 1588
[1] 3759
[1] 1865
[1] 4948
[1] 3992
[1] 2444
[1] 4539
[1] 288
[1] 3914
[1] 806
[1] 3094
[1] 244
[1] 3648
[1] 1478
[1] 2701
[1] 1971
[1] 1271
[1] 4789
[1] 517
[1] 396
[1] 4364
[1] 1935
[1] 4479
[1] 1737
[1] 910
[1] 2239
[1] 4193
[1] 2063
[1] 410
[1] 4210
[1] 3340
[1] 1060
[1] 1209
[1] 2837
[1] 3640
[1] 345
[1] 2709
[1] 4975
[1] 3918
[1] 3785
[1] 4815
[1] 2931
[1] 3710
[1] 2236
[1] 1604
[1] 2622
[1] 867
[1] 1187
[1] 2347
[1] 1428
[1] 4177
[1] 4357
[1] 2128
[1] 3624
[1] 126
[1] 2613
[1] 4404
[1] 230
[1] 4367
[1] 641
[1] 91
[1] 2434
[1] 4580
[1] 4891
[1] 2915
[1] 1859
[1] 3785
[1] 4999
[1] 1373
[1] 939
[1] 1131
[1] 4071
[1] 1307
[1] 545
[1] 3620
[1] 376
[1] 2263
[1] 1796
[1] 2168
[1] 2062
[1] 357
[1] 3297
[1] 4529
[1] 824
[1] 2550
[1] 4340
[1] 4715
[1] 3725
[1] 4990
[1] 2973
[1] 2670
[1] 581
[1] 3472
[1] 1290
[1] 4221
[1] 1080
[1] 3250
[1] 676
[1] 1946
[1] 3050
[1] 660
[1] 2361
[1] 3111
[1] 2665
[1] 549
[1] 853
[1] 3255
[1] 3790
[1] 2303
[1] 4546
[1] 4158
[1] 3922
[1] 2935
[1] 4199
[1] 4789
[1] 1919
[1] 979
[1] 1472
[1] 717
[1] 1300
[1] 4112
[1] 4595
[1] 4316
[1] 3584
[1] 4532
[1] 4988
[1] 2273
[1] 3771
[1] 1392
[1] 3295
[1] 3639
[1] 3891
[1] 2037
[1] 3571
[1] 4445
[1] 1830
[1] 4086
[1] 3776
[1] 4510
[1] 390
[1] 1130
[1] 2830
[1] 1942
[1] 39
[1] 4273
[1] 442
[1] 3079
[1] 2230
[1] 3230
[1] 2937
[1] 2268
[1] 2812
[1] 263
[1] 186
[1] 54
[1] 444
[1] 4695
[1] 3424
[1] 1188
[1] 4867
[1] 1551
[1] 1252
[1] 146
[1] 1072
[1] 4455
[1] 1200
[1] 2169
[1] 843
[1] 350
[1] 1473
[1] 4510
[1] 4415
[1] 2899
[1] 3794
[1] 897
[1] 4457
[1] 3664
[1] 106
[1] 3657
[1] 3417
[1] 4903
[1] 4782
[1] 3905
[1] 1470
[1] 4672
[1] 1350
[1] 1339
[1] 4051
[1] 2866
[1] 1130
[1] 2872
[1] 4678
[1] 2320
[1] 4617
```







```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm8.staticflickr.com/7133/7845930476_52f7934da5_o.png) 



Compute model-based warning signals on all each of these.  



```r
dt <- data.table(subset(zoom, value>threshold))
var <- dt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
acor <- dt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
dat <- melt(data.frame(Variance=var, Autocorrelation=acor))
```




### Null distribution 

To compare against the expected distribution of these statistics, we create another set of simulations without conditioning on having experienced a chance transition, on which we perform the identical analysis.  



```r
select_crashes <- function(n){
	T<- 5000
	n_pts <- n
	pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
	sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=500)
	d <- dim(sn$x1)
	sn$x1[1:501,]
}
```






```r
select_crashes <- function(n){
  n <- n/10 # doesn't need as long as the individual-based
  sn <- 
  sapply(1:1000, function(rep){
    x <- vector(mode="double", length=n)
    x[1] <- 8 # positive equilibrium
    z <- rlnorm(n, 0, .1)
    r = .75; k = 10; a=1.55; H=1; Q = 3
    for(t in 1:n){
      x[t+1] = z[t] *  x[t] * exp(r * (1 - x[t] / k) - a * x[t] ^ (Q - 1) / (x[t] ^ Q + H ^ Q)) 
    }
    x
  })
	sn[1:501,] 
}




```




```r
sfInit(parallel=TRUE, cpu=12)
sfLibrary(populationdynamics)
```

```
Library populationdynamics loaded.
```

```r
sfExportAll()
examples <-  sfLapply(1:24, function(i) select_crashes(50000))
nulldat <- melt(as.matrix(as.data.frame(examples, check.names=FALSE)))
nulldat <- melt(examples)
names(nulldat) = c("time", "reps", "value")
levels(nulldat$reps) <- 1:length(levels(dat$reps)) 
```




Zoom in on the relevant area of data near the crash



```r
require(plyr)
nullzoom <- ddply(nulldat, "reps", function(X){
    data.frame(time=X$time, value=X$value)
    })
```








```r
nulldt <- data.table(nullzoom)
nullvar <- nulldt[, warningtrend(data.frame(time=time, value=value), window_var), by=reps]$V1
nullacor <- nulldt[, warningtrend(data.frame(time=time, value=value), window_autocorr), by=reps]$V1
nulldat <- melt(data.frame(Variance=nullvar, Autocorrelation=nullacor))
```






```r
ggplot(dat) + geom_histogram(aes(value, y=..density..), binwidth=0.2, alpha=.5) +
 facet_wrap(~variable) + xlim(c(-1, 1)) + 
 geom_density(data=nulldat, aes(value), bw=0.2)
```

![plot of chunk figure2](http://farm9.staticflickr.com/8290/7845930684_54dd61daf5_o.png) 



