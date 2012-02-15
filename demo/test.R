# test.R



rm(list=ls())
require(earlywarning)


data(ibms)
A <- stability_model(ibm_critical, "OU")
B <- stability_model(ibm_critical, "LSN")


update(A, X = simulate(A))


require(snowfall)
sfInit(parallel=T, cpu=4)
sfLibrary(earlywarning)
sfExportAll()


reps <- sfLapply(1:10, function(i) compare(A,B))


lik_ratio <- 
  sapply(reps, function(rep){
      null <- -2*(logLik(rep$AA) - logLik(rep$BA))
      test <- -2*(logLik(rep$AB) - logLik(rep$BB))
      c(null=null, test=test)
  })



