


## May Model 

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
library(earlywarning)
library(reshape2)		# data manipulation
library(data.table)	# data manipulation
library(ggplot2)		# graphics
library(snowfall)		# parallel
```




```r
theme_publish <- theme_set(theme_bw(12))
theme_publish <- 
  theme_update(legend.key=theme_blank(),
        panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),
        plot.background=theme_blank(), legend.title=theme_blank())
```




### Conditional distribution

Then we fix a set of paramaters we will use for the simulation function.  Since we will simulate 20,000 replicates with 50,000 pts a piece, we can save memory by performing the conditional selection on the ones that crash as we go along and disgard the others.  (We will create a null distribution in which we ignore this conditional selection later).  


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
	sn[,crashed] 
}
```





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

![plot of chunk testing](http://farm9.staticflickr.com/8040/7925692422_bde0b54eaf_o.png) 



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
[1] 1773
[1] 601
[1] 1229
[1] 4009
[1] 3572
[1] 1703
[1] 2815
[1] 2920
[1] 1317
[1] 4032
[1] 1553
[1] 730
[1] 3213
[1] 3843
[1] 1415
[1] 252
[1] 4142
[1] 1079
[1] 2936
[1] 512
[1] 1467
[1] 1950
[1] 3864
[1] 3130
[1] 616
[1] 1896
[1] 4309
[1] 4660
[1] 2509
[1] 1915
[1] 3253
[1] 4890
[1] 796
[1] 2457
[1] 3503
[1] 1756
[1] 4235
[1] 4368
[1] 1383
[1] 2439
[1] 3621
[1] 4151
[1] 3259
[1] 2742
[1] 1344
[1] 627
[1] 1915
[1] 2734
[1] 2391
[1] 738
[1] 2704
[1] 1647
[1] 1764
[1] 134
[1] 2989
[1] 853
[1] 2311
[1] 492
[1] 865
[1] 1804
[1] 3465
[1] 3644
[1] 2528
[1] 3396
[1] 4847
[1] 2220
[1] 3174
[1] 2931
[1] 3211
[1] 2485
[1] 315
[1] 340
[1] 4322
[1] 527
[1] 2918
[1] 93
[1] 3189
[1] 4683
[1] 195
[1] 4287
[1] 330
[1] 3044
[1] 1181
[1] 2728
[1] 4764
[1] 3070
[1] 788
[1] 2067
[1] 2289
[1] 3071
[1] 4872
[1] 2858
[1] 879
[1] 3216
[1] 1088
[1] 3117
[1] 4335
[1] 1933
[1] 178
[1] 1571
[1] 3741
[1] 2196
[1] 3384
[1] 1734
[1] 3663
[1] 4612
[1] 360
[1] 4106
[1] 3643
[1] 1565
[1] 1366
[1] 2724
[1] 1842
[1] 1472
[1] 4650
[1] 3194
[1] 3833
[1] 1762
[1] 4064
[1] 437
[1] 409
[1] 1838
[1] 278
[1] 2189
[1] 1326
[1] 2450
[1] 82
[1] 4366
[1] 4416
[1] 2995
[1] 3548
[1] 3093
[1] 4737
[1] 3501
[1] 2301
[1] 1814
[1] 1961
[1] 3821
[1] 1998
[1] 4373
[1] 2318
[1] 1543
[1] 3068
[1] 2256
[1] 1923
[1] 2846
[1] 135
[1] 1170
[1] 4684
[1] 991
[1] 3956
[1] 3008
[1] 2648
[1] 1622
[1] 416
[1] 930
[1] 3452
[1] 4597
[1] 2851
[1] 1038
[1] 1197
[1] 4561
[1] 4635
[1] 3679
[1] 3701
[1] 1329
[1] 1132
[1] 4278
[1] 2689
[1] 639
[1] 4929
[1] 4027
[1] 1328
[1] 53
[1] 483
[1] 1927
[1] 4350
[1] 4352
[1] 3304
[1] 3827
[1] 524
[1] 2085
[1] 3050
[1] 3443
[1] 4761
[1] 3605
[1] 4375
[1] 388
[1] 1142
[1] 2613
[1] 447
[1] 793
[1] 1546
[1] 1530
[1] 1505
[1] 2879
[1] 4164
[1] 873
[1] 2444
[1] 4085
[1] 2751
[1] 3554
[1] 747
[1] 4345
[1] 4650
[1] 1765
[1] 582
[1] 4465
[1] 4640
[1] 4351
[1] 1546
[1] 4201
[1] 4322
[1] 947
[1] 1179
[1] 500
[1] 3399
[1] 4884
[1] 3092
[1] 128
[1] 4319
[1] 1838
[1] 224
[1] 3568
[1] 169
[1] 4364
[1] 1314
[1] 4671
[1] 688
[1] 3633
[1] 3614
[1] 3334
[1] 3271
[1] 4567
[1] 1166
[1] 626
[1] 273
[1] 3083
[1] 2056
[1] 2618
[1] 2843
[1] 4707
[1] 4846
[1] 3882
[1] 1256
[1] 3045
[1] 2357
[1] 1753
[1] 729
[1] 726
[1] 1711
[1] 128
[1] 4065
[1] 1380
[1] 4067
[1] 1624
[1] 4987
[1] 210
[1] 3732
[1] 3966
[1] 4398
[1] 3874
[1] 3931
[1] 178
[1] 1766
[1] 2298
[1] 132
[1] 4657
[1] 1070
[1] 2704
[1] 3346
[1] 4509
[1] 920
[1] 3295
[1] 1240
[1] 2924
[1] 1792
[1] 2051
[1] 4935
[1] 3248
[1] 960
[1] 2259
[1] 221
[1] 3103
[1] 1555
[1] 3154
[1] 4304
[1] 1822
[1] 3142
[1] 3714
[1] 1545
[1] 3515
[1] 4510
[1] 1850
[1] 321
[1] 3959
[1] 632
[1] 2834
[1] 2706
[1] 1448
[1] 2269
[1] 1569
[1] 3390
[1] 2927
[1] 2940
[1] 3764
[1] 66
[1] 2175
[1] 1846
[1] 3877
[1] 1864
[1] 2973
[1] 441
[1] 4477
[1] 439
[1] 4081
[1] 4321
[1] 3231
[1] 2964
[1] 978
[1] 807
[1] 833
[1] 1312
[1] 3917
[1] 4994
[1] 336
[1] 3824
[1] 2567
[1] 2135
[1] 4267
[1] 1563
[1] 2699
[1] 1119
[1] 2184
[1] 2033
[1] 2905
[1] 2525
[1] 3577
[1] 1450
[1] 256
[1] 704
[1] 3061
[1] 3285
[1] 663
[1] 2909
[1] 2196
[1] 2491
[1] 3959
[1] 1837
[1] 2599
[1] 3931
[1] 479
[1] 3444
[1] 3663
[1] 3661
[1] 1851
[1] 1995
[1] 4084
[1] 3104
[1] 2901
[1] 4901
[1] 1738
[1] 3017
[1] 4839
[1] 4673
[1] 3991
[1] 2038
[1] 4568
[1] 3400
[1] 454
[1] 1467
[1] 1928
[1] 3380
[1] 2672
[1] 4751
[1] 4886
[1] 1842
[1] 2150
[1] 2557
[1] 1021
[1] 1354
[1] 3587
[1] 3178
[1] 296
[1] 4219
[1] 3026
[1] 922
[1] 4211
[1] 4402
[1] 4426
[1] 3460
[1] 4335
[1] 4647
[1] 159
[1] 1353
[1] 3333
[1] 2810
[1] 3647
[1] 2317
[1] 348
[1] 2738
[1] 3435
[1] 4691
[1] 720
[1] 3110
[1] 4299
[1] 694
[1] 4750
[1] 2880
[1] 4824
[1] 2821
[1] 64
[1] 4145
[1] 964
[1] 2768
[1] 540
[1] 3074
[1] 399
[1] 3764
[1] 4275
[1] 1096
[1] 4943
[1] 2577
[1] 4592
```




```r
ggplot(subset(zoom, reps %in% levels(zoom$reps)[1:9])) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
```

![plot of chunk example-trajectories](http://farm9.staticflickr.com/8298/7925692690_226a3d647a_o.png) 



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

![plot of chunk figure2](http://farm9.staticflickr.com/8299/7925692880_f80ea200cd_o.png) 



