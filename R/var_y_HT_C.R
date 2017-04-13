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
#' # Sampling of population from Figure 1, Thompson (1990)
#'
#' data(Thompson1990Figure1Population)
#' data(Thompson1990Figure1Sample)
#' 
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Figure1Population, aes(x,y, 
#' 	size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Figure1Sample, aes(x,y), shape=0, size=7)
#' 
#' # INITIATE ACS
#' Z = createACS(population=Thompson1990Figure1Population, 
#' 	n1=dim(Thompson1990Figure1Sample)[1], 
#' 	initial_sample=Thompson1990Figure1Sample, y_variable="y_value")
#' 
#' # CALCULATE var(y_HT)
#' # create dataframe of network info
#' Z_summary <- Z %>% group_by(NetworkID) %>%
#' 	summarise(
#' 		m = m[1],
#' 		y_total = sum(y_value, rm.na=TRUE)
#' 		) %>%
#' 		filter(NetworkID > 0)
#' 
#' var_y_HT(
#' 	N = dim(Thompson1990Figure1Population)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1], 
#' 	m = Z_summary$m, 
#' 	y = Z_summary$y_total
#' )
#' @useDynLib ACSampling
#' @importFrom Rcpp sourceCpp
#' @export

var_y_HT <- function(N, n1, m, y, pi_i_values=NULL) {
	if (is.null(pi_i_values)) {
		pi_i_values 	<- pi_i(N, n1, m)
	}
	pi_ij_values 		<- pi_ij(N, n1, m) %>% as.matrix
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m), ncol=length(m), NA))
	# calculate for all pairs
	V <- var_y_HT_cpp(m, pi_i_values, pi_ij_values, y)
	sum(V, na.rm=T)/(N^2)
}







new_y_HT <- function(y, N, n1, m_threshold, pi_i_values=NULL, m=NULL, sampling=NULL, criterion=NULL) {
	if (!(is.null(sampling)) & !(is.null(criterion))) {
		J = ifelse(y >= criterion | sampling=="SRSWOR", 1, 0)
	} else {
		J = 1
	}
	Z = data.frame(y=y, m=m)
	
	# with replacement inclusion probability for cluster units
	# pi_i = 1 - ( 1 - ((m + a)/N) )^ n
	# how to make this work for a network
	
	A <- Z %>% filter(m <= m_threshold)
	B <- Z %>% filter(m > m_threshold)
	if (dim(A)[1] > 0) {
		A$pi_i_values = pi_i(N, n1, A$m)	
	}
	if (dim(B)[1] > 0) {	
		B$pi_i_values = pi_i(N, n1, m_threshold)
	}
	Z <- rbind.fill(A, B) %>% as.data.frame
	y_HT = sum(unlist(Z$y)*J/unlist(Z$pi_i_values), na.rm=T)/N
	return(y_HT)	
}


var_y_HT_RACS <- function(N, n1, m, y, pi_i_values=NULL) {
	pi_ij_values 		<- pi_ij(N, n1, m) %>% as.matrix
	
	
	
	
	N_m_n1 	<- sapply(m, function(m) choose(N - m, n1)) 
	N_m_m_n1 = matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	) # store binom(N-mj-mh, n1)
	
	
	
	A <- Z %>% filter(m <= m_threshold)
	B <- Z %>% filter(m > m_threshold)
	if (dim(A)[1] > 0) {
		A$pi_i_values = pi_i(N, n1, A$m)	
	}
	if (dim(B)[1] > 0) {	
		B$pi_i_values = pi_i(N, n1, m_threshold)
	}
	Z <- rbind.fill(A, B) %>% as.data.frame
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m), ncol=length(m), NA))
	# calculate for all pairs
	V <- var_y_HT_cpp(m, pi_i_values, pi_ij_values, y)
	sum(V, na.rm=T)/(N^2)
}