#' Calculate the Horvitz-Thompson Ratio Estimator
#' @description R_hat is calculated by dividing the Horvitz-Thompson estimator, Tau_hat_z by Tau_hat_x. See p. 77, Thompson (2002). THERE CANNOT BE NAS IN X OR Y?
#' @template y
#' @template x
#' @template N
#' @template n1
#' @template m_vec
#' @template replace
#' @template ex_Thompson2002_p_78
#' @export

R_hat <- function(y, x, N, n1, m_vec, replace="FALSE") {
     
     handleError_n1(n1)
     
	if (replace=="TRUE") {
		alpha_stars <- pi_i_replace(N, n1, m_vec)
	} else {
		alpha_stars <- pi_i(N, n1, m_vec)
	}
	dat <- data.frame(alpha_stars = alpha_stars, y = y, x = x)
	mu_y = sum(dat$y/dat$alpha_stars)
	if (mu_y == 0) {
		return(0)
	} else {
		sum(dat$x/dat$alpha_stars, na.rm=T) / mu_y
	}
}


