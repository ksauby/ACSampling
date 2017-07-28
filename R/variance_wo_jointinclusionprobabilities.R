total sample size?

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

Brewer 1 and 2