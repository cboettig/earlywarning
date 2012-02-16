\name{summary_statistic}
\alias{summary_statistic}
\title{apply a summary statistic over a set of replicate data}
\usage{
  summary_statistic(dat, indicator, windowsize, ...)
}
\arguments{
  \item{dat}{a data frame with columns "time", "reps", and
  "value" observed in a timeseries of replicates.}

  \item{indicator}{a function taking X and windowsize, such
  as \link{window_var}}

  \item{windowsize,}{passed to indicator function}

  \item{...}{additional optional arguments to the indicator
  function}
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

