#
require(earlywarning)

data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")
observed <- -2 * (logLik(A) - logLik(B))

require(snow)
cl <- makeCluster(32, type="MPI")
clusterEvalQ(cl, library(earlywarning))
clusterExport(cl, ls())


reps <- parLapply(cl, 1:100, function(i) compare(A,B))
stopCluster(cl)

save("reps", file="ibm_analysis.rda")
