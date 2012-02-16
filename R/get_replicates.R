#' extract the replicates simulations by model  
#' @param reps a list of outputs from compare()
#' @return a data.frame listing the "time", "rep" id number,
#' "value" observed at that time, and the "model" producing the 
#' simulated data.  
#' @seealso \link{compare}, \link{roc_data}
#' @import reshape2
#' @export 
get_replicates <- function(reps){
  ous <- sapply(reps, function(rep){
          if(rep$model=="OU")
            rep$X[[2]] 
  })
  lsns <- sapply(reps, function(rep){
          if(rep$model=="LSN")
            rep$X[[2]] 
  })
  dat <- melt(list(OU=ous, LSN=lsns))
  names(dat) <- c("time", "rep", "value", "model")
  dat$time <- reps[[1]][dat$time, 1]
  dat
}




