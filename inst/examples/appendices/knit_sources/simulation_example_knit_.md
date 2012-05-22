<!--roptions dev=png, fig.width=5, fig.height=5, warning=FALSE, comment=NA, cache.path=/simulation, tidy=FALSE-->

<!--begin.rcode setup, echo=FALSE
render_gfm() # use GFM hooks for output
opts_knit$set(base.url='https://github.com/cboettig/earlywarning/wiki/')
end.rcode-->
<!-- This loads all the libraries we need ahead of time, so they load quitely when displayed!-->
<!--begin.rcode libraries, echo=FALSE
require(populationdynamics) 
require(earlywarning)
require(plyr)
require(beanplot)
require(ggplot2)
require(reshape2)
require(doMC)
end.rcode-->


## Model-based analysis of warning signals

We load the package and the data set from the individual-based simulations.  To this data we fit both the LSN and OU models, 
<!--begin.rcode models
require(earlywarning)
data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")
observed <- -2 * (logLik(A) - logLik(B))
end.rcode-->

Comparing the models by the bootstrapping procedure is intensive.  `compare` simulates under each model, and then estimates both models on both simulated sets of data (for a total of 4 model fits).  As only the LSN fit is computationally intensive, this takes about twice the time as the above fitting step of model B.  We will need to repeat this many times to build up a bootstrap distribution. Consequently, it is convient to run this step in parallel over multiple processors or clusters of computers.   

### Parallelization of the comparison
Fortunately R has a diverse array of parallel tools,  [Schmidberger *et al.*, 2009](http://epub.ub.uni-muenchen.de/8991)) give a nice review.  Here we set up the flexible `snowfall` parallel envirnoment, which can run in serial, on a multicore chip, or over a cluster of machines.  

<!-- Code for parallelization via snowfall -->
<!--begin.rcode parallel2, include=FALSE
require(snowfall)
sfInit(par=T, cpu=16) 
sfLibrary(earlywarning)
sfExportAll()
end.rcode-->

Rather than build this choice into the earlywarning signals package, the user can choose whatever looping implementation they desire to compute replicates of the comparison. Our results are cached in the package, so anyone seeking to explore this example can run `data(example_analysis)` instead.  The actual analysis takes just one line:

<!--begin.rcode doreps, include=FALSE
reps <- sfLapply(1:500, function(i) compare(A,B))
end.rcode-->


### Visualizing the distributions

Once this step is finished, the remaining analysis is just a matter of shuffling and plotting the data.   We extract the likelihood ratios (deviances) from the null and test simulations with a simple function.  
<!--begin.rcode lr
lr <- lik_ratios(reps)
end.rcode-->

We can visualize this data as overlapping distributions
<!--begin.rcode dists
require(ggplot2)
ggplot(lr) + geom_density(aes(value, fill=simulation), alpha=.7) + 
  geom_vline(xintercept=observed, lty=2)
end.rcode-->

Or as beanplot
<!--begin.rcode beanplot
require(beanplot)
beanplot(value ~ simulation, lr, what=c(0,1,0,0))
abline(h=observed, lty=2)
end.rcode-->

### Generating the ROC Curve

We can make the ROC curve by calculating the true and false positive rates from the overlap of these distributions using `roc_data`.  This gives us a data frame of threshold values for our statistic, and the corresponding false positive rate and true positive rate.  We can simply plot those rates against each other to create the ROC curve. 

<!--begin.rcode roc
roc <- roc_data(lr)
ggplot(roc) + geom_line(aes(False.positives, True.positives), lwd=1)
end.rcode-->

### Parameter distributions

We can also look at the bootstraps of the parameters.  Another helper function will reformat this data from reps list.  The fit column uses a two-letter code to indicate first what model was used to simulate the data, and then what model was fit to the data.  For instance, AB means the data was simulated from model A (the null, OU model) but fit to B.  
<!--begin.rcode pars
pars <- parameter_bootstraps(reps)
head(pars)
end.rcode-->


There are lots of options for visualizing this relatively high-dimensional data, which we can easily explore with a few commands from the `ggplot2` package. For instance, we can look at average and range of parameters estimated in the bootstraps of each model: 
<!--begin.rcode parsplot
require(Hmisc)
ggplot(subset(pars, fit %in% c("AA", "BB")), aes(fit, value)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = median_hilow, geom = "pointrange", 
               position = position_dodge(width = 0.90), conf.int = 0.95) +
  facet_wrap(~parameter, scales="free_y")
end.rcode-->

### Visualizing the simulation replicates

The `reps` list also contains the simulation data from each replicate

<!--begin.rcode replicates, fig.width=10
sims <- get_replicates(reps)
ggplot2(sims) + geom_line(aes(time, value, group=rep), alpha=0.1) + facet_wrap(~model)
end.rcode-->



## Summary 
Repeating the analysis for other data sets used in the study.  The analysis described above is essentially six steps,  each corresponding to a single line of code. 

 1. fit the null model, 
 2. fit the test model 
 3. check the observed likelihood ratio, 
 4. bootstrap, 
 5. get likelihood ratio distributions from the bootstraps 
 6. convert the distribution into error rates.

For instance, we can run these six steps on the chemostat data with: 
<!--begin.rcode chemostat, eval=FALSE
data("chemostat")
A.chemo <- stability_model(chemostat, "OU")
B.chemo <- stability_model(chemostat, "LSN")
observed.chemo <- -2 * (logLik(A.chemo) - logLik(B.chemo))
reps.chemo <- sfLapply(1:100, function(i) compare(A.chemo,B.chemo))
lr.chemo <- lik_ratios(reps.chemo)
roc.chemo <- roc_data(lr.chemo)
end.rcode-->

We highlighted two addtional functions format the parameter data and simulation results so that they can plotted or explored further.  
<!--begin.rcode chemostatextras, eval=FALSE
pars.chemo <- parameter_bootstraps(reps.chemo)
sims.chemo <- get_replicates(reps.chemo)
end.rcode-->



<!--begin.rcode save, echo=FALSE
save(list=ls(), file = "simulation_example.rda")
end.rcode-->

<!--begin.rcode stopcluster, echo=FALSE, eval=FALSE
stopCluster(cl)
end.rcode-->


