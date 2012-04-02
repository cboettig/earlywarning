#
require(earlywarning)
require(snow)
cl <- makeCluster(60, type="MPI")
clusterEvalQ(cl, library(earlywarning))


data("chemostat")
A.chemo <- stability_model(chemostat, "OU")
B.chemo <- stability_model(chemostat, "LSN")
observed.chemo <- -2 * (logLik(A.chemo) - logLik(B.chemo))
clusterExport(cl,ls())
reps.chemo <- parLapply(cl, 1:500, function(i) compare(A.chemo, B.chemo))
lr.chemo <- lik_ratios(reps.chemo)
roc.chemo <- roc_data(lr.chemo)

save(list=ls(), file="manuscript_data.rda")


stopCluster(cl)



