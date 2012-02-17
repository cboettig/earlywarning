# 

##Simulate a dataset from the full individual, nonlinear model
rm(list=ls())
T<- 5000
n_pts <- 500000
require(populationdynamics)
# Stable example parameters
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=10000)
save("sn", "file=prosecutor.rda")
