#' calculate the trend statistic from a given indicator
#'
#' @param X the data, either a time-series, a numeric, or a matrix
#' with times in the first column and values in the second column. 
#' @param indicator a function of a numeric string of values.  Additional
#' arguments such as windowsize or explicit time vector can be passed in
#' as optional arguments.  
#' @param ... additional arguments to the indicator function
#' @param method the correlation test performed, see ?cor.test for details.
#' @seealso \link{cor.test} 
#' @details supported indicators in the package so far include:
#' \link{window_var}, \link{window_skew}, \link{window_autocorr}.  
#' 
#' Note that this function does not return the p-value
#' given by the test, as this is not meaningful in this context,
#' since the assumptions are not met by early warning signals data. 
#' @export
warningtrend <- function(X, indicator, windowsize=NULL, ...,
                        method=c("kendall", "pearson", "spearman"))
{
  # Handle alternative data formats 
  if(is.ts(X))
    X <- data.frame(as.numeric(time(X)), X@.Data)
  else if(is.null(dim(X)))
    X <- data.frame(1:length(X), X)

  npts <- length(X[["time"]])
  if(is.null(windowsize))
    windowsize <- npts/2

  # compute the indicator with the function specified
  Y <- indicator(X[["value"]], c(windowsize, ...)) 
  # apply the correclation test indicated
  method=match.arg(method)
  correlation <- cor.test(X[windowsize:npts,1], Y, method=method)
  correlation$estimate
}



