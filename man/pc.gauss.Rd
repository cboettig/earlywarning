\name{pc.gauss}
\alias{pc.gauss}
\title{cumulative density function}
\usage{
  pc.gauss(setmodel, x, x0, to, t1, pars,
    lower.tail = TRUE, log.p = FALSE)
}
\arguments{
  \item{setmodel}{a function returning mean and variance
  for the desired model class}

  \item{x0}{the initial value}

  \item{to}{initial time}

  \item{t1}{end time}

  \item{pars}{parameters passed to setmodel function}

  \item{lower.tail}{logical}

  \item{log.p}{a logical indicating whether log of the
  density is desired}
}
\value{
  the cumulative probability density for the given interval
}
\description{
  cumulative density function
}

