\name{fit_models}
\alias{fit_models}
\title{fit both constant and time-dependent model}
\usage{
  fit_models(X, ...)
}
\arguments{
  \item{X}{a timeseries object (or matrix or data-frame)}

  \item{...}{additional options taht are passed to the
  optimizer}
}
\value{
  the constant and time-dependent model estimates, the data
  input, and the initial guesses of the parameters.
}
\description{
  Estimate decent starting parameter values from data &
  fits both models
}

