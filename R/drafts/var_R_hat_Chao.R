#' Calculate var_R_hat_Chao
#' 
#' @param x Attribute data about species of interest x (e.g., abundance, presence/absence).
#' @param y Attribute data about species of interest y (e.g., abundance, presence/absence).
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param mK Number of units satisfying the ACS criterion in network $i$.
#' @examples
#' N=100
#' n1=3
#' y = c(60, 14, 1)
#' x = c(1, 1, 1)
#' mk=c(5, 2, 1)
#' R_hat(x, y, N, n1, mk, without_replacement="FALSE")
#' var_R_hat(x, y, N, n1, mk)

var_R_hat_Chao <- function(x, y, N, n1, mk, with_replacement="FALSE") {
	if (with_replacement=="FALSE") {
		pi_i_values 	<- pi_i(N, n1, mk) # inclusion probabilities
		pi_jh_values 		<- pi_jh(N, n1, mk) # joint inclusion probabilities
		R_hat_estimate	<- R_hat(x, y, N, n1, mk)
	} else {
		pi_i_values 	<- pi_i_with_replacement(N, n1, mk)
		pi_jh_values 		<- pi_jh_with_replacement(N, n1, mk)
		R_hat_estimate	<- R_hat(x, y, N, n1, mk, with_replacement="TRUE")
	}
	# replace diagonal (where h = j)
	for (j in 1:length(mk)) {
		pi_jh_values[j, j] = pi_i_values[j]
	}
	y_hat 			<- y - R_hat_estimate*x
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=x, ncol=x, NA))
	# calculate for all pairs
	for (i in 1:length(x)) {
		for (j in 1:length(x)) {		
			V[i, j] = 	y_hat[i] * y_hat[j] / pi_jh_values[i, j] * (pi_jh_values[i, j]/(pi_i_values[i] * pi_i_values[j]) - 1)
		}
	}
	# replace diagonal (where i = j)
	# diag(V) <- 0
	sum(V)/(N^2)
}

