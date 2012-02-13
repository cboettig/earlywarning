\name{LSN}
\alias{LSN}
\title{Linearized Saddle Node model}
\usage{
  LSN(Xo, to, t1, pars)
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
  Estimate the mean and variance of the linearized saddle
  node process (in which the spring constant changes over
  time)
}
\keyword{internal}

