# 

##Simulate a dataset from the full individual, nonlinear model
#rm(list=ls())
#T<- 5000
#n_pts <- 50000
#require(populationdynamics)
# Stable example parameters
#pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
#    i = 0, Da = 0, Dt = 0, p = 2)
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


# Zoom in on the relevant area of data near the crash
require(plyr)
zoom <- ddply(dat, "reps", function(X){
    tip <- min(which(X$value==0))
    index <- max(tip-800,1):tip
    data.frame(time=X$time[index], value=X$value[index])
    })
zoom <- subset(zoom, value > 300)
save("zoom", file="zoom.rda")

load("zoom.rda")
require(ggplot2)
ggplot(subset(zoom, reps < 10)) + geom_line(aes(time, value)) + facet_wrap(~reps, scales="free")
ggsave("zoom.png")
#
#

#L <- length(unique(zoom$reps))
#library(snow)
## snow method
#cluster <- makeCluster(60, type="MPI")
#clusterEvalQ(cluster, library(earlywarning)) # load a library
#clusterExport(cluster, ls()) # export everything in workspace
#models <- parLapply(cluster, 1:L, function(i)
#  stability_model(zoom[zoom$rep==i, c("time", "value")], "LSN")
#  )
#stopCluster(cluster)
#save("models", file="models.rda")


 load("zoom.rda")
 load("models.rda")
# plot indicators
require(plyr)
require(earlywarning)
indicators <- ddply(zoom, "reps", function(X){
    Y <- data.frame(time=X$time, value=X$value)
    tau_var <- warningtrend(Y, window_var)
    tau_acorr <- warningtrend(Y, window_autocorr)
    i <- X$rep[1]
    m <- models[[i]]$pars["m"]
    c(var=tau_var, acor=tau_acorr, m=m)
})

require(reshape2)
dat <- melt(indicators, id="reps")
png("fallacy.png")
require(beanplot)
beanplot(value ~ variable, data=dat, what=c(0,1,0,0), bw="nrd0")
dev.off()

ggplot(subset(dat, variable != "m.m")) + geom_histogram(aes(value)) + facet_wrap(~variable)
ggsave("tau_histograms.pdf")


require(ggplot2)
ggplot(subset(dat, variable != "m.m")) + geom_density(aes(value, fill=variable), alpha=.6)
ggsave("false_positives.png")
#require(socialR)
#upload("fallacy.png false_postives.png", script="prosecutorSims.R", tag="stochpop warningsignals")

#ggplot(indicators) + geom_density(aes(kendall_coef))
#ggplot(indicators) + geom_density(aes(m))





