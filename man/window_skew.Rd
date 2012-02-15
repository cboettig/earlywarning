\name{window_skew}
\alias{window_skew}
\title{Compute skew in a sliding window}
\usage{
  window_skew(X, windowsize = length(X)/2)
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
  Compute skew in a sliding window
}
\seealso{
  \link{warningtrend}
}

