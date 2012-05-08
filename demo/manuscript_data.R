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

save(list=ls(), file="manuscript_data.rda")


data("chemostat")
A.chemo <- stability_model(chemostat, "OU")
B.chemo <- stability_model(chemostat, "LSN")
observed.chemo <- -2 * (logLik(A.chemo) - logLik(B.chemo))
clusterExport(cl, ls())
reps.chemo <- parLapply(cl, 1:500, function(i) compare(A.chemo, B.chemo))
lr.chemo <- lik_ratios(reps.chemo)
roc.chemo <- roc_data(lr.chemo)

save(list=ls(), file="manuscript_data.rda")


data("glaciation")
A.deut <- stability_model(glaciation, "OU")
B.deut <- stability_model(glaciation, "LSN")
observed.deut <- -2 * (logLik(A.deut) - logLik(B.deut))
clusterExport(cl, ls())
reps.deut <- parLapply(cl, 1:500, function(i) compare(A.deut, B.deut))
lr.deut <- lik_ratios(reps.deut)
roc.deut <- roc_data(lr.deut)

save(list=ls(), file="manuscript_data.rda")


stopCluster(cl)



