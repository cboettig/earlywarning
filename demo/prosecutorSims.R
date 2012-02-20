# 

##Simulate a dataset from the full individual, nonlinear model
rm(list=ls())
T<- 5000
n_pts <- 50000
require(populationdynamics)
# Stable example parameters
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
## Run the individual based simulation
#sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1)
#save("sn", "file=prosecutor.rda")

#load("prosecutor.rda")
#d <- dim(sn$x1)
#crashed <- which(sn$x1[d[1],]==0)
#dat <- melt( sn$x1[,crashed] )
#names(dat) = c("time", "reps", "value")

#save("dat", file="crashed.rda")
#rm(list=ls())
load("crashed.rda")

L <- length(unique(dat$reps))

library(snow)
## snow method
cluster <- makeCluster(60, type="MPI")
clusterEvalQ(cluster, library(earlywarning)) # load a library
clusterExport(cluster, ls()) # export everything in workspace
models <- parLapply(cluster, 1:L, function(i)
  stability_model(dat[dat$rep==i, c("time", "value")], "LSN")
  )
stopCluster(cluster)
save("models", file="models.rda")



runlater <- function(){
indicators <- ddply(dat, "reps", function(X){ 
    Y <- data.frame(X$time, X$value)
    tau <- compute_tau(Y, "Var")[1]
    i <- X$rep[1]
    m <- models[[i]]$pars["m"]
    c(tau, m)
})

require(beanplot)
png("beanplot.png", width=480*2)
par(mfrow=c(1,2))
beanplot(m, indicators, what=c(0,1,0,0))
beanplot(kendall_coef, subset(indicators, abs(m) <1), what=c(0,1,0,0))
dev.off()


ggplot(indicators) + geom_density(aes(kendall_coef))
ggplot(indicators) + geom_density(aes(m))
}









