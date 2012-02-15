\name{lik_ratios}
\alias{lik_ratios}
\title{extract the likelihood ratios under both null and test simulations}
\usage{
  lik_ratios(reps)
}
\arguments{
  \item{reps}{a list of outputs from compare()}
}
\value{
  a data.frame of columns "simulation" (null or test),
  "value" (deviance between models on that simulation), and
  "rep", a replicate id number.
}
\description{
  extract the likelihood ratios under both null and test
  simulations
}
\seealso{
  \link{compare}, \link{roc_data}
}

