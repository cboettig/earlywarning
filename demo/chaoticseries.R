# generates a chaotic time series, for which early warning signal detection is not expected to work

N <- numeric(100)
N[1] <- 99

r <- 3.9
K <- 100

f <- function(N) r*N*(1-N/K)

for(t in 1:99)
	N[t+1] = f(N[t])

# visualize the time-series
plot(1:100, N)
chaos <- ts(N,start=0, deltat=1)

save(list="chaos", file="chaos.rda")


# run the analysis
require(warningsignals)
m <- fit_models(chaos, "LSN")

save(list=ls(), file="chaos.rda")

