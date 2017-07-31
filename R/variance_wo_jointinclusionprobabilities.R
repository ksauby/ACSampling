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


Hajek <- function(pi_i, N) {
	pi_i * (1 - pi_i) * N / (N - 1)
}


#' Variance estimator free of joint inclusion probability calculations for unequal probability sampling
#' @param N Population size
#' @param y Vector of $y$ values.
#' @param pi_i_values vector of first-order inclusion probabilities, calculated using \code{pi_i}.
#' @param estimator Options include "Hartley_Rao", "Hajek", "Rosen", "Berger", and "Deville".
#' @references Hajek, J. (1964). Asymptotic theory of rejective sampling with varying probabilities from a finite population. The Annals of Mathematical Statistics.
#' Berger, Y. G., & Tille, Y. (2009). Sampling with Unequal Probabilities. Handbook of Statistics, 1–17.
#' @export

var_pi <- function(N, y, pi_i_values, estimator) {
	#n <- length(y)
	#if (length(y) != length(pi_i_values)) {
	#	stop("y and pi_i must be of equal length.")
	#}
	#if (estimator == "Hartley_Rao") {
	#	alpha_i_values <- Hartley_Rao(pi_i_values, n)
	#	lambda_i_values <- 1
	#}
	if (estimator == "Hajek") {
		lambda_i_values <- alpha_i_values <- Hajek(pi_i_values, N)
	}
	#if (estimator == "Rosen") {
	#	lambda_i_values <- alpha_i_values <- Rosen(pi_i_values, n)
	#}
	#if (estimator == "Berger") {
	#}
	#if (estimator == "Deville") {
	#	lambda_i_values <- alpha_i_values <- Deville(pi_i_values, n)
	#}
	#alpha_i <- eval(parse(text=alpha_i))
	
	B_hat <- sum(lambda_i_values * y/pi_i_values) / 
		sum(lambda_i_values)
	epsilon_i <- y/pi_i_values - B_hat
	sum(alpha_i_values * epsilon_i^2)
	
	
}