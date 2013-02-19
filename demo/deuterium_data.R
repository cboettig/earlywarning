# Glaciation data
# Good practice shouldn't require this in a file in data/ directory
require(warningsignals)


avereps <- function(X, ID){
  unique_id <- ID[!duplicated(ID)]
  out <- sapply(unique_id, function(id) c(id, mean(X[ID==id,2])))
  t(out)
}

# read in the raw data from: 
deut <- read.table("deutnat.txt")

# divisions from: Dakos et. al.
g1 <- which(-deut$V2 > -58800 & -deut$V2 < -12000)
p1 <- which(-deut$V2 > -58800 & -deut$V2 < -17000)
glaciationI   <- data.frame("time"=-deut$V2[p1], "data"=deut$V3[p1])
g2 <- which(-deut$V2 > -151000 & -deut$V2 < -128000)
p2 <- which(-deut$V2 > -151000 & -deut$V2 < -135000)
glaciationII   <- data.frame("time"=-deut$V2[p2], "data"=deut$V3[p2])
g3 <- which(-deut$V2 > -270000 & -deut$V2 < -238000)
p3 <- which(-deut$V2 > -270000 & -deut$V2 < -242000)
glaciationIII   <- data.frame("time"=-deut$V2[p3], "data"=deut$V3[p3])
g4 <- which(-deut$V2 > -385300 & -deut$V2 < -324600)
p4 <- which(-deut$V2 > -385300 & -deut$V2 < -334100)
glaciationIV   <- data.frame("time"=-deut$V2[p4], "data"=deut$V3[p4])

# Data in original time units
glaciation <- list(glaciationI, glaciationII, glaciationIII, glaciationIV)

# Data in time since start, with replicates averaged out
mydata <- vector("list", 4)
for(i in 1:4){
	X <- glaciation[[i]]
	X <- data.frame("time"=rev(X[,1] - min(X[,1])), "data"=rev(X[,2]))
	X <-avereps(X, ID=X[,1])  ### NEED A NEW AVEREPS FUNCTION!!
	mydata[[i]] <- X
}

for(i in 1:4){
	mydata[[i]] <- dakos_data_processing(mydata[[i]])
  #plot(data[[i]])
}
#for(i in 1:4) plot_kendalls(data[[i]]$X_ts)

## Clean up namespace by removing extra objects?
rm(list=c("g1", "p1", "g2", "p2", "g3", "p3", "g4", "p4", "glaciation", "glaciationI", "glaciationII", "glaciationIII", "glaciationIV", "avereps", "deut", "i", "X"))

# As timeseries objects
deuterium <- lapply(mydata, function(x) x$X_ts)
rm(list="mydata")

# data() will load the R file in preference to the .rda file anyway
save(list="deuterium", file="deuterium.rda")
