
## SHOW THAT: autocorrelation doesn't always suck.  


## Run the individual based simulation
require(populationdynamics) 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, i = 0, Da = .09, Dt = 0, p = 2)
time=seq(0, 500, length=500)
sn <- saddle_node_ibm(pars,time)
X <- data.frame(time=time, value=sn$x1)

# observed value
require(earlywarning)
observed <- warningtrend(X, window_autocorr) 

## fit the models 
A <- stability_model(X, "OU")

## simulate  
reps <- 100
Asim <- simulate(A, reps)

# tidy up the data a bit 
require(reshape2)
Asim <- melt(Asim, id="time")
names(Asim)[2] <- "rep"

# Apply the warningtrend "window_autocorr" to each replicate
require(plyr)
wsA <- ddply(Asim, "rep", warningtrend, window_autocorr, .progress="text")

## Repeat for B
B <- stability_model(X, "LSN")
Bsim <- simulate(B, reps)
Bsim <- melt(Bsim, id="time")
names(Bsim)[2] <- "rep"
wsB <- ddply(Bsim, "rep", warningtrend, window_autocorr, .progress="text")


## plot
tidy <- melt(data.frame(null=wsA$tau, test=wsB$tau))
names(tidy) <- c("simulation", "value")
require(beanplot)
beanplot(value ~ simulation, tidy, what=c(0,1,0,0))
abline(h=observed, lty=2)




## Here's how this would look in parallel 
require(doMC)
registerDoMC()
wsA <- ddply(Asim, "rep", warningtrend, window_autocorr, .parallel=TRUE)
wsB <- ddply(Bsim, "rep", warningtrend, window_autocorr, .parallel=TRUE)

## In principle this should scale to snow/MPI mode but it doesn't....
#http://stackoverflow.com/questions/5559287/how-do-i-make-dosmp-play-nicely-with-plyr
#http://www.numbertheory.nl/2011/11/14/parallelization-using-plyr-loading-objects-and-packages-into-worker-nodes/

library(snow)
library(doSNOW)
cl <- createCluster(2, export = ls(), lib = list("earlywarning"))
ws <- ddply(Asim, "rep", warningtrend, window_autocorr, .parallel=TRUE)
stopCluster(cl)


# The classic way to do this doesn't work either.  
makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
clusterEvalQ(cl, library(earlywarning)) # load a library
clusterExport(cl, ls()) # export everything in workspace
ws <- ddply(Asim, "rep", warningtrend, window_autocorr, 
            .progress="text", .parallel=TRUE)
stopCluster(cl)

