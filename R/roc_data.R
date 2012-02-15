#' Compute data to draw an ROC curve
#'
#' Grab the data for the ROC curve from the null & test distributions
#' @param dat a data.frame with columns "simulation", indicating "null" or "test",
#' "value", with the value of the test statistic, and "rep" indicating the replicate number
#' Or set dat=NULL and specify null_dist and test_dist directly, see below.  
#' @param pts (optional) number of output points desired, defaults to 50, 
#' should not be more than the number of points in each distribution
#' @param null_dist a distribution (vector of samples) of the test statistic 
#' under the null hypothesis, if not given as a data frame
#' @param test_dist the values under the test hypothesis
#' @return a data frame with thresholds, false positive and 
#' corresponding true positive rates
#' @export
roc_data <- function(dat, pts=50, null_dist=NULL, test_dist=NULL, silent=TRUE){
  # Creates the ROC curve from the output of the bootstrap function

  if(!is.null(dat)){
    null_dist <- subset(dat, simulation=="null")$value
    test_dist <- subset(dat, simulation=="test")$value
  }

  n_null <- length(null_dist)
  n_test <- length(test_dist)


  lower <- min(null_dist, test_dist)
  upper <- max(null_dist, test_dist)
  threshold <- seq(lower, upper, length=pts)

  # Calculate the ROC curve
  roc <- sapply(threshold, 
                function(thresh){
                  c(sum(null_dist > thresh)/n_null,
                  sum(test_dist > thresh)/n_test)
                })
  roc <- t(roc)

  # Caculate the AUC
  f<-approx(roc[,1], roc[,2], n=200) 
  delta <- f$x[2] - f$x[1]
  area  <- sum(f$y*delta)

  # display some summary information
  if(!silent){
    message(paste("Area Under Curve = ", area))
    id <- which(roc[,1]<=.05)[1]
    message(paste("True positive rate = ", roc[id,2], "at false positive rate of", roc[id,1]))
  }
  out <- cbind(threshold,roc)
  out <- as.data.frame(out)
  names(out) <- c("Threshold", "False.positives", "True.positives")
  out
}





