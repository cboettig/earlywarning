\name{window_kurtosi}
\alias{window_kurtosi}
\title{Compute kurtosis}
\usage{
  window_kurtosi(X, windowsize = length(X)/2)
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
  Compute kurtosis
}
\details{
  Please note that kurtosis has never been proposed as an
  indicator, though some have since mentioned it in this
  context.
}
\seealso{
  \link{warningtrend}
}

