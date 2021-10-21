#' Calculate the variance of R hat
#' 
#' @param y Attribute data about object of interest y (e.g., abundance, presence/absence).
#' @param x Auxiliary data about object of interest y.
#' @param N Population size.
#' @param n1 An integer giving the initial sample size (ie., the size of the initial simple random sample).
#' @param m Number of units satisfying the ACS criterion in network $i$.
#' @param replace Whether sampling should be done with or without replacement. Defaults to \code{FALSE}.
#' @examples
#' # Example from Thompson (2002), p. 78-79
#' N = 100
#' n1 = 4
#' y = c(60, 60, 14, 1)
#' x = c(1, 1, 1, 1)
#' m = c(5, 5, 2, 1)
#' R_hat(y, x, N, n1, m, replace="TRUE")
#' var_R_hat(y, x, N, n1, m)
#' @export


var_R_hat <- function(y, x, N, n1, m, replace="FALSE") {
	if (replace=="TRUE") {
		pi_i_values <- pi_i_replace(N, n1, m)		
		pi_ij_values <- pi_ij_replace(N, n1, m)
	} else {
		pi_i_values <- pi_i(N, n1, m)
		pi_ij_values <- pi_ij(N, n1, m)
	}
	R_hat_estimate	<- R_hat(y, x, N, n1, m)
	y_hat 			<- y - R_hat_estimate*x
	# get information on unique networks
	# calculate for all pairs
	V <- R_hat_cpp(y_hat, pi_i_values, pi_ij_values)
	# replace diagonal (where i = j)
	# diag(V) <- 0
	(1/(N^2)) * (sum(
		((y_hat)^2) * (1/(pi_i_values^2) - 1/pi_i_values), na.rm=T
	) + sum(V, na.rm=T)/2)
}

