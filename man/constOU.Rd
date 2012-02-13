\name{constOU}
\alias{constOU}
\title{OU model for stable system}
\usage{
  constOU(Xo, to, t1, pars)
}
\arguments{
  \item{Xo}{initial condition}

  \item{to}{initial time}

  \item{t1}{final time}

  \item{pars}{numeric of parameters named Ro, theta, and
  sigma}
}
\value{
  a list with values Ex, the expected x value, and Vx, the
  expected variance
}
\description{
  Estimate the appropriate mean and variance of the OU
  process
}
\keyword{internal}

