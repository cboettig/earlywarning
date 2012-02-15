\name{window_ar.ols}
\alias{window_ar.ols}
\title{Compute autocorrelation over a sliding window by a least squares approach}
\usage{
  window_ar.ols(X, windowsize = length(X)/2,
    demean = FALSE)
}
\arguments{
  \item{X}{a numeric containing evenly sampled values from
  a time series}

  \item{windowsize}{the size of the sliding time window (in
  # of pts) used to compute the statistic.}
}
\value{
  a numeric of size length(X)-windowsize of values of the
  statistic calculated in the sliding window.
}
\description{
  Compute autocorrelation over a sliding window by a least
  squares approach
}
\seealso{
  \link{warningtrend}, \link{ar.ols}
}

