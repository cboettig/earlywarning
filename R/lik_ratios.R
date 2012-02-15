#' extract the likelihood ratios under both null and test simulations
#' @param reps a list of outputs from compare()
#' @return a data.frame of columns "simulation" (null or test), 
#' "value" (deviance between models on that simulation), 
#' and "rep", a replicate id number.  
#' @seealso \link{compare}, \link{roc_data}
#' @import reshape2
#' @export 
lik_ratios <- function(reps){
  dat <- sapply(reps, function(rep){
          null <- -2*(logLik(rep$AA) - logLik(rep$BA))
          test <- -2*(logLik(rep$AB) - logLik(rep$BB))
          c(null=null, test=test)
  })
  dat <- melt(dat)
  names(dat) <- c("simulation", "rep", "value") 
  dat
}




