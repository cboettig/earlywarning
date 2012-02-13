\name{rc.gauss}
\alias{rc.gauss}
\title{random number draw from a gauss model}
\usage{
  rc.gauss(setmodel, n = 1, x0, to, t1, pars)
}
\arguments{
  \item{setmodel}{a function returning mean and variance
  for the desired model class}

  \item{n}{replicates}

  \item{x0}{the initial value}

  \item{to}{initial time}

  \item{t1}{end time}

  \item{pars}{parameters passed to setmodel function}
}
\value{
  a normal random variate with mean and sd determined by
  setmodel
}
\description{
  random number draw from a gauss model
}
\details{
  (called internally by full names despite being written in
  S3-style notation)
}

