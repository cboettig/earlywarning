#' parameteric bootstrap the correlation coefficient under both hypotheses
#' 
#' @param X data sampling time and sampled value as columns. If only
#' one column is provided, assumes that the time is uniform
#' @param indicator a function that returns the warning indicator over time,
#' the correlation of the resulting series with time is returned parameter
#' @param ... additional arguments to \link{warningtrend}
#' @param reps number of replicate simulations to use.
#' @return  data.frame with a column indicating the model used (null or test)
#' and a column with the requested correlation coefficient, see \link{cor.test}
#' @seealso \link{warningtrend}
#' @examples
#' data(ibm)
#' bootstrap_trend(ibm_critical, window_var, method="pearson", rep=10)
#' @export
bootstrap_trend(X, indicator, ..., reps=100){
  A <- stability_model(X, "OU")
  B <- stability_model(X, "LSN")
  Asim <- simulate(A, reps)
  Bsim <- simulate(B, reps)

  Asim <- melt(Asim, id="time")
  Bsim <- melt(Bsim, id="time")
  names(Asim)[2] <- "rep"
  names(Bsim)[2] <- "rep"

  wsA <- ddply(Asim, "rep", warningtrend, indicator, ...)
  wsB <- ddply(Bsim, "rep", warningtrend, indicator, ...)
  tidy <- melt(data.frame(null=wsA[[2]], test=wsB[[2]]))
  names(tidy) <- c("simulation", "value")
  tidy
}
