#' Calculate joint inclusion probability of unit $j$ and $h$ for with replacement sampling
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Vector of values giving the number of units satisfying the ACS criterion in network $i$.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples 
#' 
#' N = 10
#' n1 = 3
#' m = c(2,3)
#' # create matrix of all pairwise pi_jh's
#' pi_jh(N, n1, m)

pi_jh_with_replacement <- function(N, n1, m) {
	pi_ = as.data.frame(matrix(nrow = length(m), ncol = length(m), NA)) # store pi_jh's
	for (j in 1 : length(m)) {
		for (h in 1 : length(m)) {
  	    	# create pi_jh's
  	    	pi_[j, h] <- 1 - {
				(1 - m[i]/N)^n1 + (1 - m[j]/N)^n1 - (1 - (m[i] + m[j])/N)^n1
		  	}
	  	}
	}
	return(pi_)
}