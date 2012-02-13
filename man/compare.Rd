\name{compare}
\alias{compare}
\title{Bootstrap parameters and likelihood ratios}
\usage{
  compare(A, B)
}
\arguments{
  \item{A}{a fitted model with update and simulate methods}

  \item{B}{another model capable of fitting the same data
  object}
}
\value{
  a list with:

  AA the model A fitted to simulations from A

  BB model B fitted to simulations from B

  AB the model A fitted to simulations from B

  BA model B fitted to simulations from A

  likelihood estimates,

  parameter values
}
\description{
  Simulate under both models and fit results to simulation
}
\details{
  will repeat simulation and fitting if one of the model
  updates fails to converge on that data
}

