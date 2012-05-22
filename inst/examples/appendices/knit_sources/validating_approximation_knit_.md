<!--roptions dev=png,fig.width=5,fig.height=5,warning=FALSE,cache.path=validating/, comment=NA-->

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

In the case of the simulated model, we can compare our process of simulating under the approximate models, OU and LSN, to what we would have gotten if we could simulate under the true process.  


<!--begin.rcode load, echo=FALSE
require(earlywarning)
nreps <- 100
end.rcode-->

Now we create the replicates by simulating under the original model.  We will not try to estimate the *nonlinear* model first, as we generally do not have the power to do this well (but see [my notes](http://www.carlboettiger.info/archives/461) attempting this anyway).  


<!--begin.rcode simulation
require(populationdynamics) 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, i = 0, Da = .09, Dt = 0, p = 2)
time=seq(0, 500, length=500)
sn <- saddle_node_ibm(pars,time, reps=nreps)
X <- data.frame(time=time, value=sn$x1)
end.rcode-->

We tidy up the data a bit
<!--begin.rcode tidy
require(reshape2)
dat <- melt(sn$x1)
names(dat) = c("time", "rep", "population")
end.rcode-->


We can set up a parallel envirnoment for a multicore machine,
<!--begin.rcode parallel, include=FALSE
require(doMC)
registerDoMC()
end.rcode-->

We can now fit the LSN model to each of these replicates, 
<!--begin.rcode fit
require(plyr)
models <- dlply(dat, "rep", 
  function(X){
    Y <- data.frame(X$time, X$population)
    stability_model(Y, "LSN")
  }, 
  .parallel=TRUE)
end.rcode-->

and look at the distribution of parameters.  

<!--begin.rcode getpars
p <- sapply(models, `[[`, "pars")
end.rcode-->


We compare this to simulating replicates from the *approximate* model and restimating parameters.  We will use the model average parameters as the parameters for a model that will generate our simulations.  

<!--begin.rcode modelmean 
p_ave <- rowMeans(p)
B <- models[[1]]
B$pars <- p_ave
end.rcode-->

We simulate \Sexpr{nreps} as before,

<--begin.rcode simulate2
Y <- simulate(B, nreps)
dat_approx <- melt(Y)
names(dat_approx) = c("time", "rep", "population")
end.rcode-->

And now we are ready to estimate again:
<!--begin.rcode fit2
models_approx <- dlply(dat_approx, "rep", function(X){
  Y <- data.frame(X$time, X$population)
  stability_model(Y, "LSN")
}, .parallel=TRUE)
p_approx <- sapply(models_approx, `[[`, "pars")
end.rcode-->


We can just compare these to get a sense of how did.  
<!--begin.rcode 
p <- data.frame(t(p))
p_approx <- data.frame(t(p_approx))
sapply(p, mean)
sapply(p, var)
sapply(p_approx, mean)
sapply(p_approx, var)

save(list=ls(), file="validating.rda")
end.rcode-->

