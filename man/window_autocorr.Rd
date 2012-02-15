\name{window_autocorr}
\alias{window_autocorr}
\title{Compute autocorrelation over a sliding window}
\usage{
  window_autocorr(X, windowsize = length(X)/2)
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
  Compute autocorrelation over a sliding window
}
\seealso{
  \link{warningtrend}, \link{acf}
}

