\name{window_cv}
\alias{window_cv}
\title{Compute the coefficient of variation (var/mean) in a sliding window}
\usage{
  window_cv(X, windowsize = length(X)/2)
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
  Compute the coefficient of variation (var/mean) in a
  sliding window
}
\seealso{
  \link{warningtrend}
}

