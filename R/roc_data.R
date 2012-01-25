#' Compute data to draw an ROC curve
#'
#' Grab the data for the ROC curve from the null & test distributions
#' @param a distribution (vector of samples) of the test statistic 
#' under the null hypothesis
#' @param the values under the test hypothesis
#' @param pts (optional) number of output points desired, defaults to 50, 
#' should not be more than the number of points in each distribution
#' @return a data frame with thresholds, false positive and 
#' corresponding true positive rates
#' @export
roc_data <- function(null_dist, test_dist, pts=50){
  # Creates the ROC curve from the output of the bootstrap function
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
  message(paste("Area Under Curve = ", area))
  id <- which(roc[,1]<=.05)[1]
  message(paste("True Pos Prob = ", roc[id,2], "at false pos rate of", roc[id,1]))

  out <- cbind(threshold,roc)
  names(out) <- c("threshold", "False Positive", "True Positive")
  as.data.frame(out)
}


