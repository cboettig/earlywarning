\name{stability_model}
\alias{stability_model}
\title{stability model fitting}
\usage{
  stability_model(X, model = c("LSN", "OU"), p = NULL, ...,
    store_data = TRUE)
}
\arguments{
  \item{X}{data sampling time and sampled value as columns.
  If only one column is provided, assumes that the time is
  uniform}

  \item{model}{the model used}

  \item{p}{initial parameter guesses, if NULL, will guess
  from data}

  \item{...}{additional parameters passed to the optim
  routine}

  \item{store_data}{if FALSE will not return an object with
  a copy of the original data}
}
\value{
  a gauss model fit object (list), containing the data (if
  requested), the model type used, the maximum-likelihood
  estimated parameters, the log-likelihood at those values,
  and a convergence indicator
}
\description{
  stability model fitting
}
\details{
  depends on the gauss class of functions and the model
  definitions LSN and constOU.
}

