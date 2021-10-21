#' Calculate the inclusion probability of unit $i$ in a simple random sample with replacement. 
#' 
#' @template N
#' @template n1
#' @param m Vector of values giving the number of units satisfying the ACS criterion in netwalpha_star_iork $i$.
#' @return vector of inclusion probabilities
#' @template SaubyCitation

pi_i_replace <- function(N, n1, m) {
  sapply(m, function(m) 1 - (1 - m/N)^n1)
}