#
require(earlywarning)
require(snow)
cl <- makeCluster(60, type="MPI")
clusterEvalQ(cl, library(earlywarning))

data("glaciation")
A.deut <- stability_model(glaciation, "OU")
B.deut <- stability_model(glaciation, "LSN")
observed.deut <- -2 * (logLik(A.deut) - logLik(B.deut))
clusterExport(cl, ls())
reps.deut <- parLapply(cl, 1:500, function(i) compare(A.deut, B.deut))
lr.deut <- lik_ratios(reps.deut)
roc.deut <- roc_data(lr.deut)

save(list=ls(), file="glaciation.rda")


stopCluster(cl)



