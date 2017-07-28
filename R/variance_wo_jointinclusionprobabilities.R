# total sample size?


#' # Initiate ACS
#' Z = createACS(Thompson1990Figure1Population, seed=2, n1=10, "y_value", condition=0)



var_pi <- function(N, n1, m, y, pi_i_values, alpha_i, z_i) {
	pi_i_values <- pi_i(N, n1, m)
	n <- length(y)
	if (length(y) != length(pi_i_values)) {
		stop("y and pi_i must be of equal length.")
	}
	alpha_i <- eval(parse(text=alpha_i))
	B_hat <- sum(alpha_i(pi_i_values, n) * z_i * y/pi_i_values) / 
		sum(alpha_i(pi_i_values, n) * z_i^2)
	epsilon_i <- y/pi_i_values - B_hat * z_i
	sum(alpha_i * epsilon_i^2)
}

Hartley_Rao <- function(pi_i, n) {
	1 - pi_i * (n - 1)/n
}

Hajek <- function(pi_i, n) {
	(1 - pi_i) * n / (n - 1)
}

Rosen <- function(pi_i, n) {
	(1 - pi_i) * log10(1 - pi_i) / pi_i
}

Berger <- function(pi_i, n) {
	A <- pi_i * (1 - pi_i)
	A <- sum(A)
	
	B <- 1 - pi_i
	B <- sum(B)
	
	(1 - pi_i) * n * (n - 1) * B / A
}

Deville <- function(pi_i, n) {
	A <- 1 - pi_i
	A <- sum(A)
	
	D <- pi_i * (1 - pi_i)
	D <- sum(D)
	
	1 / (1 - pi_i) * (1 - (1/D^2) * A)
}

# Brewer 1 and 2