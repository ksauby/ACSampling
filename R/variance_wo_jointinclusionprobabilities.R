#' Calculate the variance of the Horvitz-Thompson estimator of the mean
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @param N Population size
#' @param n1 Initial sample size
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param y Vector of $y$ total, each corresponding to a unique network.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @examples 
#' library(ggplot2)
#' library(magrittr)
#' library(dplyr)

Hajek <- function(pi_i, n) {
	pi_i * (1 - pi_i) * n / (n - 1)
}


#' Calculate the variance of the Horvitz-Thompson estimator of the mean
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @param N Population size
#' @param n1 Initial sample size
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param y Vector of $y$ total, each corresponding to a unique network.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @examples 
#' library(ggplot2)
#' library(magrittr)
#' library(dplyr)

var_pi <- function(n, pi_i_values, y, alpha_i, lambda_i, z_i) {
	#n <- length(y)
	#if (length(y) != length(pi_i_values)) {
	#	stop("y and pi_i must be of equal length.")
	#}
	if (alpha_i == "Hartley_Rao") {
		alpha_i_values <- Hartley_Rao(pi_i_values, n)
		lambda_i_values <- 1
	}
	if (alpha_i == "Hajek") {
		lambda_i_values <- alpha_i_values <- Hajek(pi_i_values, n)
	}
	if (alpha_i == "Rosen") {
		lambda_i_values <- alpha_i_values <- Rosen(pi_i_values, n)
	}
	if (alpha_i == "Berger") {
	}
	if (alpha_i == "Deville") {
		lambda_i_values <- alpha_i_values <- Deville(pi_i_values, n)
	}
	#alpha_i <- eval(parse(text=alpha_i))
	
	B_hat <- sum(lambda_i_values * z_i * y/pi_i_values) / 
		sum(lambda_i_values * z_i^2)
	epsilon_i <- y/pi_i_values - B_hat * z_i
	sum(alpha_i_values * epsilon_i^2)
	
	
}