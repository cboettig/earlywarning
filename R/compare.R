#' Bootstrap parameters and likelihood ratios 
#'
#' Simulate under both models and fit results to simulation
#' @param A a fitted model with update and simulate methods
#' @param B another model capable of fitting the same data object
#' @return a list with: 
#' @return AA the model A fitted to simulations from A 
#' @return BB model B fitted to simulations from B
#' @return AB the model A fitted to simulations from B 
#' @return BA model B fitted to simulations from A
#' @return likelihood estimates, 
#' @return parameter values
#' @details will repeat simulation and fitting if one of the model updates
#' fails to converge on that data
#' @export
compare <- function(A, B){
  done <- 0
  while(!done){
    Asim <- simulate(A)
    AfitA <- update(A, X = Asim)
    BfitA <- update(B, X = Asim)
    done <- converge(AfitA) && converge(BfitA)
  }
  done <- 0
  while(!done){
    Bsim <- simulate(B)
    BfitB <- update(B, X = Bsim)
    AfitB <- update(A, X = Bsim)
    done <- converge(AfitB) && converge(BfitB)
  }
  list(AA = AfitA, BB = BfitB, AB = AfitB, BA = BfitA)
}


