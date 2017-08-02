#' Hakey variance estimator free of joint inclusion probability calculations for unequal probability sampling
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @param N Population size
#' @param n1 Initial sample size
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param y Vector of $y$ total, each corresponding to a unique network.
#' @description Option for the "var_pi" function.
#' @references Hajek, J. (1964). Asymptotic theory of rejective sampling with varying probabilities from a finite population. The Annals of Mathematical Statistics.
#' Berger, Y. G., & Tille, Y. (2009). Sampling with Unequal Probabilities. Handbook of Statistics, 1–17.
#' @export



Hajek <- function(pi_i, n) {
	pi_i * (1 - pi_i) * n / (n - 1)
}


#' Variance estimator free of joint inclusion probability calculations for unequal probability sampling, Hajek
#' @param n sample size
#' @param y Vector of $y$ values.
#' @param pi_i_values vector of first-order inclusion probabilities, calculated using \code{pi_i}.
#' @references Berger, Y. G. (2005). Variance estimation with Chao's sampling scheme. Journal of Statistical Planning and Inference, 127(1-2), 253–277. http://doi.org/10.1016/j.jspi.2003.08.014
#' @export

var_Hajek <- function(n, y, pi_i_values) {
	d_hat <- sum(1 - pi_i_values)
	G_hat <- 1/d_hat * sum( (y/pi_i_values) * (1 - pi_i_values) )
	
	n/(n - 1) * sum( (1 - pi_i_values) * (y/pi_i_values - G_hat)^2 )
}


#' Variance estimator free of joint inclusion probability calculations for unequal probability sampling
#' @param n sample size
#' @param y Vector of $y$ values.
#' @param pi_i_values vector of first-order inclusion probabilities, calculated using \code{pi_i}.
#' @param estimator Options include "Hartley_Rao", "Hajek", "Rosen", "Berger", and "Deville".
#' @references Hajek, J. (1964). Asymptotic theory of rejective sampling with varying probabilities from a finite population. The Annals of Mathematical Statistics.
#' Berger, Y. G., & Tille, Y. (2009). Sampling with Unequal Probabilities. Handbook of Statistics, 1–17.
#' @export
#' @examples
#' # Hajek Approximation
#' Z = createACS(Thompson1990Figure1Population, seed=3, n1=30, "y_value", condition=0)
#' Z_summary <- Z %>% 
#' 	filter(Sampling!="Edge") %>%
#' 	group_by(NetworkID) %>%
#' 	filter(NetworkID > 0) %>%
#' 	dplyr::summarise(
#' 		m = m[1],
#' 		y_total = sum(y_value, na.rm=TRUE)
#' 	)
#' var_y_HT(
#' 	N = dim(Thompson1990Figure1Population)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1], 
#' 	m = Z_summary$m, 
#' 	y = Z_summary$y_total
#' )
#' pi_i_values <- pi_i(N=900,n1=30, m=Z_summary$m)
#' Hajek(pi_i=pi_i_values, N=900)
#' var_pi(
#' 	n = 30, 
#' 	y = Z_summary$y_total, 
#' 	pi_i_values = pi_i_values,
#' 	estimator = "Hajek"
#' )


var_pi <- function(n, y, pi_i_values, estimator) {
	if (estimator == "Hajek") {
		lambda_i_values <- alpha_i_values <- Hajek(pi_i_values, n)
	}
	
	B_hat <- sum(lambda_i_values * y/pi_i_values) / 
		sum(lambda_i_values)
	e_i <- y/pi_i_values - B_hat
	
	sum(alpha_i_values * e_i^2)
	#if (estimator == "Hartley_Rao") {
	#	alpha_i_values <- Hartley_Rao(pi_i_values, n)
	#	lambda_i_values <- 1
	#}
	#if (estimator == "Rosen") {
	#	lambda_i_values <- alpha_i_values <- Rosen(pi_i_values, n)
	#}
	#if (estimator == "Berger") {
	#}
	#if (estimator == "Deville") {
	#	lambda_i_values <- alpha_i_values <- Deville(pi_i_values, n)
	#}
}