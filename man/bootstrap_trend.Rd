\name{bootstrap_trend}
\alias{bootstrap_trend}
\title{parameteric bootstrap the correlation coefficient under both hypotheses}
\usage{
  bootstrap_trend(X, indicator, ..., reps = 100)
}
\arguments{
  \item{X}{data sampling time and sampled value as columns.
  If only one column is provided, assumes that the time is
  uniform}

  \item{indicator}{a function that returns the warning
  indicator over time, the correlation of the resulting
  series with time is returned parameter}

  \item{...}{additional arguments to \link{warningtrend}}

  \item{reps}{number of replicate simulations to use.}
}
\value{
  data.frame with a column indicating the model used (null
  or test) and a column with the requested correlation
  coefficient, see \link{cor.test}
}
\description{
  parameteric bootstrap the correlation coefficient under
  both hypotheses
}
\examples{
data(ibm)
bootstrap_trend(ibm_critical, window_var, method="pearson", rep=10)
}
\seealso{
  \link{warningtrend}
}

