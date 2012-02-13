\name{roc_data}
\alias{roc_data}
\title{Compute data to draw an ROC curve}
\usage{
  roc_data(null_dist, test_dist, pts = 50)
}
\arguments{
  \item{a}{distribution (vector of samples) of the test
  statistic under the null hypothesis}

  \item{the}{values under the test hypothesis}

  \item{pts}{(optional) number of output points desired,
  defaults to 50, should not be more than the number of
  points in each distribution}
}
\value{
  a data frame with thresholds, false positive and
  corresponding true positive rates
}
\description{
  Grab the data for the ROC curve from the null & test
  distributions
}

