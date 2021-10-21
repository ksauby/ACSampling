#' Calculate the variance of the Horvitz-Thompson estimator of the mean
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @template N
#' @template n1
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param y Vector of $y$ total, each corresponding to a unique network.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @useDynLib ACSampling
#' @importFrom Rcpp sourceCpp
#' @export
#' @examples 
#' library(ggplot2)
#' library(magrittr)
#' library(dplyr)
#' # Sampling of population from Figure 1, Thompson (1990)
#'
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Figure1Sample)
#' 
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, 
#' 	size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Figure1Sample, aes(x,y), shape=0, size=7)
#' 
#' # INITIATE ACS
#' Z = createACS(population_data=Thompson1990Fig1Pop, 
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
#' 	N = dim(Thompson1990Fig1Pop)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1], 
#' 	m = Z_summary$m, 
#' 	y = Z_summary$y_total
#' )


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


#' Calculate the variance of the Horvitz-Thompson estimator of the mean using the RACS correction
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @template N
#' @template n1
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param y Vector of $y$ total, each corresponding to a unique network.
#' @param m_threshold threshold value above which to calculate pi_i and pi_j differently.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @useDynLib ACSampling
#' @importFrom Rcpp sourceCpp
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' library(dplyr)
#' # Sampling of population from Figure 1, Thompson (1990)
#'
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Figure1Sample)
#' 
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, 
#' 	size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Figure1Sample, aes(x,y), shape=0, size=7)
#' 
#' # INITIATE ACS
#' Z = createACS(popdata = Thompson1990Fig1Pop, n1 = dim(Thompson1990Figure1Sample)[1], initsample = Thompson1990Figure1Sample, yvar = "y_value")
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
#' var_y_HT_RACS(
#' 	N = dim(Thompson1990Fig1Pop)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1], 
#' 	m = Z_summary$m, 
#' 	y = Z_summary$y_total,
#' 	m_threshold=3
#' )

var_y_HT_RACS <- function(N, n1, m, y, m_threshold, pi_i_values=NULL) {
	Z = data.frame(y=y, m=m) %>% arrange(m)
	A <- Z %>% filter(m <= m_threshold)
	B <- Z %>% filter(m > m_threshold)
	if (dim(A)[1] > 0) {
		A$pi_i_values = pi_i(N, n1, A$m)	
	}
	if (dim(B)[1] > 0) {	
		B$pi_i_values = pi_i(N, n1, m_threshold)
	}
	Z <- bind_rows(A, B) %>% as.data.frame
	pi_i_values 		<- Z$pi_i_values
	pi_ij_values 		<- pi_ij_RACS(N, n1, m, m_threshold) %>% as.matrix
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- Z$pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m), ncol=length(m), NA))
	# calculate for all pairs
	V <- var_y_HT_cpp(m, pi_i_values, pi_ij_values, y)
	sum(V, na.rm=T)/(N^2)
}
