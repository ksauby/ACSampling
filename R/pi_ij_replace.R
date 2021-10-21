#' Calculate joint inclusion probability of unit $j$ and $h$
#' @template N
#' @template n1
#' @param m Vector of values giving the number of units satisfying the ACS criterion in network $i$.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

pi_ij_replace <- function(N, n1, m) {
	pi_ij = matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	)
	for (i in 1 : length(m)) {
		for (j in 1 : length(m)) {
			pi_ij[i, j] <- 
			1 - (
				(1 - m[i]/N)^n1 + 
				(1 - m[j]/N)^n1 -
				 (1 - (m[i] + m[j])/N)^n1
				)
		}
	}
	return(pi_ij)
}