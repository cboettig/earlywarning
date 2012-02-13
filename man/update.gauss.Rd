\name{update.gauss}
\alias{update.gauss}
\title{update routine for gaussian model fits}
\usage{
  \method{update}{gauss} (object, ...)
}
\arguments{
  \item{object}{any gauss-class object}

  \item{...}{additional arguments, see details.  Must at
  least include X=}
}
\value{
  an updated gauss-class object depending on other
  parameters given
}
\description{
  update routine for gaussian model fits
}
\details{
  data has to be passed in as X= via the ..., see example.
  any commands to the optim routine used are also passed in
  in this manner. one can also add the option store_data =
  FALSE to avoid returning the input data, see
  \code{\link{stability_model}} for details.
}

