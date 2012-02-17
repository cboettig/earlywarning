
#' apply a summary statistic over a set of replicate data
#' 
#' @param dat a data frame with columns "time", "reps", and "value"
#' observed in a timeseries of replicates.   
#' @param indicator a function taking X, such as \link{window_var}
#' @param ... additional optional arguments to the indicator function,
#' such as windowsize
#' @return a data frame with columns "time", "reps", and "value",
#' where values are the sliding-window computed summary statistics produced
#' by indicator.  
#' @details helper function just helps simple indicator functions be 
#' applied to data formated with replicate observations and still
#' return nicely formatted data 
#' @import plyr
#' @import reshape2
#' @export
summary_statistic <- function(dat, indicator, ...){
  tmp <- ddply(dat, "reps", function(X) 
    indicator(X$value, ...))
  stat <- melt(t(tmp))
  stat <- stat[-1,]
  names(stat) <- c("time", "reps", "value")
  stat$time <- as.numeric(gsub("\\D", "", stat$time))
  class(stat$value) <- "numeric"
  stat
}

