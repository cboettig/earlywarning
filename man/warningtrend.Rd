\name{warningtrend}
\alias{warningtrend}
\title{calculate the correlation coefficient of some summary statistic over time}
\usage{
  warningtrend(X, indicator, windowsize = NULL, ...,
    method = c("kendall", "pearson", "spearman"))
}
\arguments{
  \item{X}{the data, either a time-series, a numeric, or a
  matrix with times in the first column and values in the
  second column.}

  \item{indicator}{a function of a numeric string of
  values.  Additional arguments such as windowsize or
  explicit time vector can be passed in as optional
  arguments.}

  \item{...}{additional arguments to the indicator
  function}

  \item{method}{the correlation test performed, see
  ?cor.test for details.}
}
\description{
  calculate the correlation coefficient of some summary
  statistic over time
}
\details{
  supported indicators in the package so far include:
  \link{window_var}, \link{window_skew},
  \link{window_autocorr}.

  Note that this function does not return the p-value given
  by the test, as this is not meaningful in this context,
  since the assumptions are not met by early warning
  signals data.
}
\seealso{
  \link{cor.test}
}

