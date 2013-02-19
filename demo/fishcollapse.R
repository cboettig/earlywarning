# file fishcollapse.R
# Author Carl Boettiger <cboettig@gmail.com>
# Date: 10 Nov 2011
# Description: an example trying to detect early warning signals
#  in data from fisheries collapses


require(warningsignals)
require(ggplot2)
require(reshape2)
# Load the data #
# ------------- #
scotia <- read.csv("../data/rawdata/sau_scotia.csv")

# Visualize data #
# ------------- #
dat_scotia <- melt(scotia, id="Year")
p_scotia <- ggplot(subset(dat_scotia, variable=="Atlantic.cod"), aes(Year, value, fill=variable)) + 
            geom_area()
print(p_scotia)

window_var <- function(X, windowsize=(length(X)/2)){
    out <- sapply(0:(length(X)-windowsize), function(i){
              var(X[(i+1):(i+windowsize)]) 
                })
     c(rep(NA, length(X)-length(out)), out)
}

window_autocorr <- function(X, windowsize=(length(X)/2)){
  out <-sapply(0:(length(X)-windowsize), 
            function(i) 
              acf(X[(i+1):(i+windowsize)], lag.max=1, plot=F)$acf[2])
     c(rep(NA, length(X)-length(out)), out)
}

require(data.table)

fish <- data.table(subset(dat_scotia, Year < 1992))
require(earlywarning)
tmp <- data.frame(species = fish$variable, Year = fish$Year, Stock = fish$value,
                  variance = fish[, window_var(value), by="variable"]$V1,
                  acor = fish[, window_autocorr(value), by="variable"]$V1)
            
dat <- melt(tmp, id=c("Year", "species"))
ggplot(subset(dat, species %in% c("Atlantic.cod"))) +  geom_point(aes(Year, value)) + facet_grid(variable~species, scales="free_y")

ggplot(subset(dat, species %in% c( "American.lobster"))) +  geom_point(aes(Year, value)) + facet_grid(variable~species, scales="free_y")

dt <- data.table(dat_scotia)
indicator <- data.frame(dt[, window_var(value), by="variable"], Year = dat_scotia$Year)
ggplot(indicator) + geom_line(aes(Year, V1)) + facet_wrap(~variable, scales="free_y")













m <- fit_models(cod.ts, "LSN")

# decreasing stability? 
m$timedep$pars$m < 0 


## analysis of summary statistics
#taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, cpu=4, nboot=20))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, cpu=4, nboot=20))
plot(mc, main="Warning signals in cod?")


