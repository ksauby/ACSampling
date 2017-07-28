# total sample size?


#' # Initiate ACS
#' Z = createACS(Thompson1990Figure1Population, seed=3, n1=30, "y_value", condition=0)

var_pi(
	N = dim(Thompson1990Figure1Population)[1],
	n1 = 30, 
	m = Z %>% filter(Sampling!="Edge") %$% m, 
	y = Z %>% filter(Sampling!="Edge") %$% y_value, 
	alpha_i = "Hajek", 
	z_i = 1
)

Z_summary <- Z %>% 
	filter(Sampling!="Edge") %>%
	group_by(NetworkID) %>%
	summarise(
		m = m[1],
		y_total = sum(y_value, rm.na=TRUE)
		) %>%
		filter(NetworkID > 0)

var_y_HT(
	N = dim(Thompson1990Figure1Population)[1], 
	n1 = dim(Thompson1990Figure1Sample)[1], 
	m = Z_summary$m, 
	y = Z_summary$y_total
)


var_pi <- function(N, n1, m, y, alpha_i, z_i) {
	pi_i_values <- pi_i(N, n1, m)
	n <- length(y)
	if (length(y) != length(pi_i_values)) {
		stop("y and pi_i must be of equal length.")
	}
	alpha_i <- eval(parse(text=alpha_i))
	alpha_i_values <- alpha_i(pi_i_values, n)
	B_hat <- sum(alpha_i_values * z_i * y/pi_i_values) / 
		sum(alpha_i_values * z_i^2)
	epsilon_i <- y/pi_i_values - B_hat * z_i
	sum(alpha_i_values * epsilon_i^2)
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