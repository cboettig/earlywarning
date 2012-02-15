\name{get_replicates}
\alias{get_replicates}
\title{extract the replicates simulations by model}
\usage{
  get_replicates(reps)
}
\arguments{
  \item{reps}{a list of outputs from compare()}
}
\value{
  a data.frame listing the "time", "rep" id number, "value"
  observed at that time, and the "model" producing the
  simulated data.
}
\description{
  extract the replicates simulations by model
}
\seealso{
  \link{compare}, \link{roc_data}
}

