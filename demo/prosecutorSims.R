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

# These sims take a while to run and about 3.7 Gigs of memory to store
# the reshape/plyr tools won't work very happily on them, so lets do some data management.
require(data.table)
dat <- data.table(sn$x1)
rm(sn) # cleanup

## show what we got
tables()

#dat[,]
#crashed <- which(sn$x1[d[1],]==0)


