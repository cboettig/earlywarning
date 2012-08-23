

# Code for Prosecutors Fallacy 
<!--

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


-->


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
ggplot(subset(dat, reps %in% levels(dat$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk testing](http://farm9.staticflickr.com/8287/7846795140_d5d810125a_o.png) 



Zoom in on the relevant area of data near the crash



```r
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value<threshold))
print(tip)
    index <- max(tip-200,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
```

```
[1] 3738
[1] 1245
[1] 2131
[1] 4025
[1] 2204
[1] 4860
[1] 1166
[1] 1671
[1] 908
[1] 3862
[1] 1522
[1] 3761
[1] 4686
[1] 1787
[1] 520
[1] 3305
[1] 470
[1] 2969
[1] 3016
[1] 1823
[1] 1175
[1] 1386
[1] 1486
[1] 3505
[1] 4815
[1] 2044
[1] 2763
[1] 3272
[1] 3826
[1] 4526
[1] 4649
[1] 4789
[1] 4796
[1] 4961
[1] 4428
[1] 2148
[1] 3286
[1] 2269
[1] 2409
[1] 3050
[1] 1199
[1] 1565
[1] 4983
[1] 1338
[1] 1027
[1] 3543
[1] 2700
[1] 292
[1] 1027
[1] 2963
[1] 2938
[1] 1123
[1] 2685
[1] 4327
[1] 2908
[1] 1581
[1] 1261
[1] 3820
[1] 158
[1] 2272
[1] 3601
[1] 4239
[1] 3250
[1] 2157
[1] 2164
[1] 3325
[1] 4828
[1] 1645
[1] 1157
[1] 107
[1] 3221
[1] 3274
[1] 1445
[1] 2621
[1] 3320
[1] 3812
[1] 3752
[1] 752
[1] 2305
[1] 4523
[1] 4397
[1] 1262
[1] 2137
[1] 3527
[1] 2622
[1] 1879
[1] 2382
[1] 3372
[1] 3651
[1] 2566
[1] 889
[1] 3924
[1] 1222
[1] 4266
[1] 111
[1] 1049
[1] 34
[1] 4406
[1] 4116
[1] 1493
[1] 4006
[1] 4640
[1] 4782
[1] 3838
[1] 4907
[1] 1767
[1] 3879
[1] 3365
[1] 4419
[1] 1188
[1] 2827
[1] 2195
[1] 1102
[1] 2534
[1] 3882
[1] 1001
[1] 3069
[1] 3131
[1] 156
[1] 895
[1] 1584
[1] 3197
[1] 3862
[1] 2200
[1] 332
[1] 3925
[1] 1205
[1] 2470
[1] 4526
[1] 987
[1] 3469
[1] 3994
[1] 4102
[1] 2045
[1] 378
[1] 197
[1] 4448
[1] 3882
[1] 1998
[1] 139
[1] 399
[1] 1896
[1] 3962
[1] 4769
[1] 4425
[1] 4434
[1] 3326
[1] 1433
[1] 2720
[1] 314
[1] 4528
[1] 4345
[1] 1042
[1] 4708
[1] 4586
[1] 3512
[1] 298
[1] 4180
[1] 1299
[1] 3935
[1] 2833
[1] 4365
[1] 4061
[1] 3125
[1] 3397
[1] 402
[1] 3141
[1] 3014
[1] 2648
[1] 3869
[1] 2654
[1] 4297
[1] 3273
[1] 185
[1] 2202
[1] 4336
[1] 4016
[1] 677
[1] 2576
[1] 88
[1] 4457
[1] 551
[1] 248
[1] 1865
[1] 2196
[1] 1065
[1] 1156
[1] 1712
[1] 126
[1] 4179
[1] 3289
[1] 2294
[1] 1885
[1] 2362
[1] 1932
[1] 863
[1] 4433
[1] 4243
[1] 3794
[1] 2361
[1] 3441
[1] 4086
[1] 4874
[1] 3586
[1] 2809
[1] 3223
[1] 4957
[1] 2297
[1] 497
[1] 4551
[1] 673
[1] 1695
[1] 1000
[1] 3922
[1] 2026
[1] 4705
[1] 4895
[1] 2246
[1] 3896
[1] 2466
[1] 402
[1] 4576
[1] 1591
[1] 1005
[1] 3651
[1] 1119
[1] 3630
[1] 112
[1] 4541
[1] 435
[1] 2883
[1] 2183
[1] 4678
[1] 3068
[1] 2117
[1] 1837
[1] 4521
[1] 3543
[1] 3414
[1] 98
[1] 3907
[1] 2190
[1] 3367
[1] 980
[1] 862
[1] 4729
[1] 4470
[1] 1430
[1] 2664
[1] 764
[1] 1832
[1] 2362
[1] 3824
[1] 4513
[1] 2559
[1] 3917
[1] 1454
[1] 1518
[1] 4612
[1] 3640
[1] 2504
[1] 3932
[1] 2983
[1] 1201
[1] 2586
[1] 810
[1] 4882
[1] 3565
[1] 2144
[1] 3394
[1] 3821
[1] 571
[1] 4530
[1] 2668
[1] 3346
[1] 28
[1] 2981
[1] 3281
[1] 2439
[1] 495
[1] 1096
[1] 2283
[1] 2096
[1] 3761
[1] 4955
[1] 1761
[1] 909
[1] 2778
[1] 4462
[1] 877
[1] 1517
[1] 3714
[1] 3489
[1] 3983
[1] 1375
[1] 923
[1] 1538
[1] 1212
[1] 4770
[1] 3443
[1] 4795
[1] 2446
[1] 4520
[1] 3710
[1] 2470
[1] 4237
[1] 2152
[1] 3027
[1] 4188
[1] 4815
[1] 4314
[1] 1283
[1] 365
[1] 3444
[1] 2090
[1] 1069
[1] 2167
[1] 1235
[1] 3673
[1] 38
[1] 3649
[1] 2224
[1] 735
[1] 3708
[1] 3809
[1] 1376
[1] 734
[1] 4382
[1] 4590
[1] 350
[1] 103
[1] 3425
[1] 4507
[1] 449
[1] 2623
[1] 4659
[1] 3939
[1] 2753
[1] 4098
[1] 3669
[1] 3149
[1] 3779
[1] 3421
[1] 1103
[1] 4018
[1] 3567
[1] 4994
[1] 3245
[1] 1550
[1] 442
[1] 2524
[1] 2733
[1] 604
[1] 136
[1] 1528
[1] 4661
[1] 4435
[1] 2820
[1] 3880
[1] 3372
[1] 2579
[1] 1375
[1] 2083
[1] 490
[1] 231
[1] 4491
[1] 3475
[1] 2744
[1] 3811
[1] 2202
[1] 4753
[1] 1761
[1] 1033
[1] 2095
[1] 2083
[1] 3523
[1] 4122
[1] 3746
[1] 3743
[1] 3839
[1] 3144
[1] 1448
[1] 257
[1] 3079
[1] 1032
[1] 2349
[1] 1753
[1] 1897
[1] 1827
[1] 84
[1] 1333
[1] 3711
[1] 2221
[1] 3258
[1] 1586
[1] 955
[1] 4629
[1] 3335
[1] 469
[1] 522
[1] 930
[1] 2879
[1] 884
[1] 3706
[1] 607
[1] 4129
[1] 180
[1] 2569
[1] 3081
[1] 3125
[1] 1336
[1] 1879
[1] 3189
[1] 3423
[1] 2997
[1] 1112
[1] 2959
[1] 2067
[1] 2011
[1] 4974
[1] 2520
[1] 3942
[1] 1527
[1] 3707
[1] 3730
[1] 1625
[1] 2918
[1] 2920
[1] 1393
[1] 4489
[1] 4043
```







```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm8.staticflickr.com/7139/7846795368_6b701a7fe5_o.png) 



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
	sn[1:201,] 
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

![plot of chunk figure2](http://farm9.staticflickr.com/8435/7846795570_87ac2e71fd_o.png) 



