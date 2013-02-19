require(warningsignals)
# CaCO3 record
caco3 <- read.table("../data/caco3.txt")
# labels, time is in millions of years Before Present, so make negative
caco3 <- data.frame("MYYrs"=-caco3$V1, "CaCO3"=caco3$V2)
## window the data
g_ca <- caco3$MYYrs >= -39 & caco3$MYYrs <= -32  # Data with collapse (for plot)
p_ca <- caco3$MYYrs >= -39 & caco3$MYYrs < -34  # Data used in warning signal
X <- data.frame("time"=caco3$MYYrs[p_ca], "data"=caco3$CaCO3[p_ca])
# Rather annoying to have time backwards and negative, lets reverse this.
X <- data.frame("time"=rev(X[,1] - min(X[,1])), "data"=rev(X[,2]))
dat <- dakos_data_processing(X)
## show interpolation and detrending
#plot.dakos(dat)

# rename focal, processed data (a timeseries object) as CaCO3
CaCO3 <- dat$X_ts

# cleanup 
rm(list=c("caco3", "g_ca", "p_ca", "X", "dat"))

