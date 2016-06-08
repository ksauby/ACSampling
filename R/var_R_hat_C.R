#' Calculate the variance of R hat
#' 
#' @param z Attribute data about species of interest z (e.g., abundance, presence/absence).
#' @param y Attribute data about species of interest y (e.g., abundance, presence/absence).
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Number of units satisfying the ACS criterion in network $i$.
#' @param with_replacement Whether sampling should be done with or without replacement. Defaults to \code{FALSE}.
#' @examples
#' # Example from Thompson (2002), p. 78-79
#' N = 100
#' n1 = 4
#' y = c(60, 60, 14, 1)
#' z = c(1, 1, 1, 1)
#' m = c(5, 5, 2, 1)
#' R_hat(y, z, N, n1, m, with_replacement="TRUE")
#' var_R_hat(y, z, N, n1, m)
#' @export


var_R_hat <- function(y, z, N, n1, m, with_replacement="FALSE") {
	if (with_replacement=="TRUE") {
		pi_i_values <- pi_i_with_replacement(N, n1, m)		
		pi_ij_values <- pi_ij_with_replacement(N, n1, m)
	} else {
		pi_i_values <- pi_i(N, n1, m)
		pi_ij_values <- pi_ij(N, n1, m)
	}
	R_hat_estimate	<- R_hat(y, z, N, n1, m)
	y_hat 			<- y - R_hat_estimate*z
	# get information on unique networks
	# calculate for all pairs
	V <- R_hat_cpp(y_hat, pi_i_values, pi_ij_values)
	# replace diagonal (where i = j)
	# diag(V) <- 0
	(1/(N^2)) * (sum(
		((y_hat)^2) * (1/(pi_i_values^2) - 1/pi_i_values), na.rm=T
	) + sum(V, na.rm=T)/2)
}

