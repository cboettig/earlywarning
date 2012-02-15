#
require(earlywarning)

data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")
observed <- -2 * (logLik(A) - logLik(B))

require(snow)
cl <- makeCluster(2, type="MPI")
clusterEvalQ(cl, library(earlywarning))
clusterExport(cl, ls())


reps <- lapply(1:2, function(i) compare(A,B))

reps <- parLapply(cl, 1:2, function(i) compare(A,B))

stopCluster(cl)

save("reps", file="ibm_analysis.rda")
