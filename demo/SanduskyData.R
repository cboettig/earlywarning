# Data analyzed by Guttal et al in 2008 Eco Let paper
# Note that data is not assumed to contain a warning signal, as Lay Sandusky in # Lake Erie has not exhibited a crash / eutriphication yet.  
# Care to make a prediction about it's phosphorous levels?

# (Even so, these may not be expected to oscillate, but could rather be the 
# constant, linear driver of a transtion...)


# Note that data is not sampled at constant interval


require(warningsignals)
lake <- read.csv("../data/SanduskyData.csv")
phospho <- data.frame(time = lake[[1]], P=lake[[6]])
