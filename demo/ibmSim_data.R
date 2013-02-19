##Simulate a dataset from the full individual, nonlinear model
T<- 500
n_pts <- 40
require(warningsignals)

## Collapsing example parameters
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200,
    i = 0, Da = .09, Dt = 0, p = 2)

## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
# format output as timeseries
ibm_critical <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])

# Stable example parameters
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
# format output as timeseries
ibm_stable <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])

# Resulting data
save(list=c("ibm_critical", "ibm_stable"), file="ibms_short.rda")



# m <-  fit_models(ibm_critical, "LSN")
# ## decreasing stability? 
# m$timedep$pars$m < 0 
## run test
# mc <- remove_unconverged(montecarlotest(m$const, m$timedep, cpu=4, nboot=20))

