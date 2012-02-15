\name{roc_data}
\alias{roc_data}
\title{Compute data to draw an ROC curve}
\usage{
  roc_data(dat, pts = 50, null_dist = NULL,
    test_dist = NULL)
}
\arguments{
  \item{dat}{a data.frame with columns "simulation",
  indicating "null" or "test", "value", with the value of
  the test statistic, and "rep" indicating the replicate
  number Or set dat=NULL and specify null_dist and
  test_dist directly, see below.}

  \item{pts}{(optional) number of output points desired,
  defaults to 50, should not be more than the number of
  points in each distribution}

  \item{null_dist}{a distribution (vector of samples) of
  the test statistic under the null hypothesis, if not
  given as a data frame}

  \item{test_dist}{the values under the test hypothesis}
}
\value{
  a data frame with thresholds, false positive and
  corresponding true positive rates
}
\description{
  Grab the data for the ROC curve from the null & test
  distributions
}

