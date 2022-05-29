#' Calculate the variance of R hat
#' 
#' @template y
#' @template x
#' @template N
#' @template n1
#' @template m_vec
#' @template replace
#' # #examples #template ex_Thompson2002_p_78
#' @export


var_R_hat <- function(y, x, N, n1, m_vec, replace="FALSE") {
	if (replace=="TRUE") {
		pi_i_values <- pi_i_replace(N, n1, m_vec)		
		pi_ij_values <- pi_ij_replace(N, n1, m_vec)
	} else {
		pi_i_values <- pi_i(N, n1, m_vec)
		pi_ij_values <- pi_ij(N, n1, m_vec)
	}
	R_hat_estimate	<- R_hat(y, x, N, n1, m_vec)
	x_hat 			<- x - R_hat_estimate*y
	# get information on unique networks
	# calculate for all pairs
	V <- R_hat_cpp(x_hat, pi_i_values, pi_ij_values)
	# replace diagonal (where i = j)
	# diag(V) <- 0
	(1/(N^2)) * (sum(
		((x_hat)^2) * (1/(pi_i_values^2) - 1/pi_i_values), na.rm=T
	) + sum(V, na.rm=T)/2)
}

