

## Potential indicator functions

#' Compute variance over a sliding window
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}
#' @export
window_var <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		var(X[(i+1):(i+windowsize)]) 
	})
}

#' Compute the coefficient of variation (var/mean) in a sliding window
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}
#' @export
window_cv <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
			var(X[(i+1):(i+windowsize)]) / mean(X[(i+1):(i+windowsize)]) 
		})
}

#' Compute mean over a sliding window
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}
#' @export
window_mean <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		mean(X[(i+1):(i+windowsize)]) 
	})
}

#' Compute skew in a sliding window
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}
#' @import psych
#' @export
window_skew <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		skew(X[(i+1):(i+windowsize)]) 
	})
}

#' Compute kurtosis 
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}
#' @import psych
#' @export
#' @details Please note that kurtosis has never been proposed as an indicator,
#' though some have since mentioned it in this context.  
window_kurtosi <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		kurtosi(X[(i+1):(i+windowsize)]) 
	})
}


#' Compute autocorrelation over a sliding window 
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}, \link{acf}
#' @export
window_autocorr <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), 
		function(i){
			a<- acf(X[(i+1):(i+windowsize)], lag.max=1, plot=F) 
			a$acf[2]
		}
	)
}

#' Compute autocorrelation over a sliding window by a least squares approach 
#' @param X a numeric containing evenly sampled values from a time series
#' @param windowsize the size of the sliding time window (in # of pts) used to 
#' compute the statistic.  
#' @return a numeric of size length(X)-windowsize of values of the statistic 
#' calculated in the sliding window.  
#' @seealso \link{warningtrend}, \link{ar.ols}
#' @export
window_ar.ols <- function(X, windowsize=length(X)/2, demean=FALSE){
	sapply(0:(length(X)-windowsize), 
		function(i){
			a<-ar.ols(X[(i+1):(i+windowsize)], demean=demean)
			a$ar[1]
		}
	)
}


