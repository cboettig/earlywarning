#
require(earlywarning)
require(snow)
cl <- makeCluster(60, type="MPI")
clusterEvalQ(cl, library(earlywarning))

data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")
observed <- -2 * (logLik(A) - logLik(B))
clusterExport(cl, ls())
reps <- parLapply(cl, 1:500, function(i) compare(A,B))
lr <- lik_ratios(reps)
roc <- roc_data(lr)

save(list=ls(), file="simulation.rda")


stopCluster(cl)



