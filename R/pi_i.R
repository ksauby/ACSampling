#' Calculate the inclusion probability of unit $i$ in a simple random sample without replacement. 
#' 
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Vector of values giving the number of units satisfying the ACS criterion in network $i$.
#' @return vector of inclusion probabilities
#' @examples 
#' # example from section 4, p. 1055 (first column) in Thompson (1990)
#' N = 5
#' n1 = 2
#' y = c(1,10,1000)
#' m = c(1,2,2)
#' pi_i(N, n1, m)


#' # EXAMPLE 4:
#' # Ch. 24, Exercise #2, p. 307, from Thompson (2002)
#' N=1000
#' n1=100
#' m=c(2,3,rep(1,98))
#' head(pi_i(N, n1, m))
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson. 1990.
#' @export

pi_i <- function(N, n1, m) {
  sapply(m, function(m) 
    1 - exp(
		sum(log({N - m - n1 + 1} : {N - m})) - sum(log({N - n1 + 1} : N))
		)
  )
}