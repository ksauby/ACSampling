#' Calculate the inclusion probability of unit $i$ in a simple random sample with replacement. 
#' 
#' @param N Population size.
#' @param n1 An integer giving the initial sample size (ie., the size of the initial simple random sample).
#' @param m Vector of values giving the number of units satisfying the ACS criterion in netwalpha_star_iork $i$.
#' @return vector of inclusion probabilities
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

pi_i_replace <- function(N, n1, m) {
  sapply(m, function(m) 1 - (1 - m/N)^n1)
}