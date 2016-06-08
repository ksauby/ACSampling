#' Calculate the variance of R hat
#' 
#' @param x Attribute data about species of interest x (e.g., abundance, presence/absence).
#' @param y Attribute data about species of interest y (e.g., abundance, presence/absence).
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param mK Number of units satisfying the ACS criterion in network $i$.
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
		pi_ij_values <- pi_ij_with_replacement(N, n1, unique(m))
	} else {
		pi_i_values <- pi_i(N, n1, m)
		pi_ij_values <- pi_ij(N, n1, unique(m))
	}
	R_hat_estimate	<- R_hat(y, z, N, n1, m)
	y_hat 			<- y - R_hat_estimate*z
	# get information on unique networks
	dat <- data.frame(y_hat = y_hat, z = z, pi_i_values = pi_i_values)
	dat <- unique(dat)
	V = as.data.frame(matrix(nrow=nrow(dat), ncol=nrow(dat), NA))
	# calculate for all pairs
	for (i in 1:nrow(dat)) {
		for (j in 1:nrow(dat)) {
			V[i, j] = 	2 * dat$y_hat[i] * dat$y_hat[j] * 
						(
							1/(dat$pi_i_values[i] * dat$pi_i_values[j]) - 
							1/pi_ij_values[i, j]
						)
		}
	}
	# replace diagonal (where i = j)
	diag(V) <- 0
	(1/(N^2)) * 
		(sum(((dat$y_hat)^2) * (1/(dat$pi_i_values^2) - 1/dat$pi_i_values), na.rm=T) + sum(V, na.rm=T)/2)
}

