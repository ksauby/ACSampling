#' Calculate the Horvitz-Thompson Ratio Estimator
#' @description R_hat is calculated by dividing the Horvitz-Thompson estimator, Tau_hat_z by Tau_hat_x. See p. 77, Thompson (2002). THERE CANNOT BE NAS IN X OR Y?
#' @param y Attribute data about species of interest y (e.g., abundance, presence/absence).
#' @param x Auxiliary data about object of interest y.
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Number of units satisfying the ACS criterion in network $i$.
#' @param replace Whether sampling should be done with or without replacement. Defaults to \code{FALSE}.
#' @examples
#' # Example from Thompson (2002), p. 78-79
#' N = 100
#' n1 = 4
#' y = c(60, 14, 1)
#' x = c(1, 1, 1)
#' m = c(5, 2, 1)
#' R_hat(y, x, N, n1, m, replace="TRUE")
#' var_R_hat(y, x, N, n1, m, replace="TRUE")
#' @export

R_hat <- function(y, x, N, n1, m, replace="FALSE") {
     
     handleError_n1(n1)
     
	if (replace=="TRUE") {
		alpha_stars <- pi_i_replace(N, n1, m)
	} else {
		alpha_stars <- pi_i(N, n1, m)
	}
	dat <- data.frame(alpha_stars = alpha_stars, y = y, x = x)
	mu_x = sum(dat$x/dat$alpha_stars)
	if (mu_x == 0) {
		return(0)
	} else {
		sum(dat$y/dat$alpha_stars, na.rm=T) / mu_x
	}
}


