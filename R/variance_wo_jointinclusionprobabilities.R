
Hajek <- function(pi_i, n) {
	pi_i * (1 - pi_i) * n / (n - 1)
}







######################################################################################







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
#' Z = createACS(Thompson1990Figure1Population, seed=3, n1=30, "y_value", condition=0)

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


Hajek(pi_i=pi_i_values, n=900)

pi_i_values <- pi_i(N=900,n1=30, m=Z$m)

var_pi(
	n = 900, 
	y = Z$y_value, 
	pi_i_values = pi_i_values,
	alpha_i = "Hajek", 
	z_i = 1
)


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