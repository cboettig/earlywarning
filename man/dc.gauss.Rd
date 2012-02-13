\name{dc.gauss}
\alias{dc.gauss}
\title{probability density}
\usage{
  dc.gauss(setmodel, x, x0, to, t1, pars, log = FALSE)
}
\arguments{
  \item{setmodel}{a function returning mean and variance
  for the desired model class}

  \item{x0}{the initial value}

  \item{to}{initial time}

  \item{t1}{end time}

  \item{pars}{parameters passed to setmodel function}

  \item{log}{a logical indicating whether log of the
  density is desired}
}
\value{
  the probability density for the given interval
}
\description{
  probability density
}

