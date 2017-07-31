






Hartley_Rao(pi_i = pi_i_values, n = N)
# negative numbers
Rosen(pi_i = pi_i_values, n = N)
# really large numbers
Berger(pi_i = pi_i_values, n = N)
# negative numbers
Deville(pi_i = pi_i_values, n = N)



# Hartley and Rao Example

Hartley_Rao_data <- data.frame(
	y_i = c(
		19,9,17,14,21,22,27,35,20,15,
		18,37,12,47,27,25,25,13,19,12
	),
	x_i = c(
		18,9,14,12,24,25,23,24,17,14,
		18,40,12,30,27,26,21,9,19,12
	)
)
pi_i_values <- Hartley_Rao_data$x_i/(sum(Hartley_Rao_data$x_i))


# total sample size?




#' # Initiate ACS
#' 

Hartley_Rao <- function(pi_i, n) {
	1 - pi_i * (n - 1)/n
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