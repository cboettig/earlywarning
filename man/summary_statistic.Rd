\name{summary_statistic}
\alias{summary_statistic}
\title{apply a summary statistic over a set of replicate data}
\usage{
  summary_statistic(dat, indicator, ...)
}
\arguments{
  \item{dat}{a data frame with columns "time", "reps", and
  "value" observed in a timeseries of replicates.}

  \item{indicator}{a function taking X, such as
  \link{window_var}}

  \item{...}{additional optional arguments to the indicator
  function, such as windowsize}
}
\value{
  a data frame with columns "time", "reps", and "value",
  where values are the sliding-window computed summary
  statistics produced by indicator.
}
\description{
  apply a summary statistic over a set of replicate data
}
\details{
  helper function just helps simple indicator functions be
  applied to data formated with replicate observations and
  still return nicely formatted data
}

