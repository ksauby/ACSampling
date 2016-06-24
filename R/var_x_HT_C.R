#' Calculate the variance of the Horvitz-Thompson estimator of the mean
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @param N Population size
#' @param n1 Initial sample size
#' @param m Vector of $m$, each corresponding to a unique network.
#' @param x Vector of $x$ total, each corresponding to a unique network.
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
#' # CALCULATE var(x_HT)
#' # create dataframe of network info
#' Z_summary <- Z %>% group_by(NetworkID) %>%
#' 	summarise(
#' 		m = m[1],
#' 		y_total = sum(y_value, rm.na=TRUE)
#' 		) %>%
#' 		filter(NetworkID > 0)
#' 
#' var_x_HT(
#' 	N = dim(Thompson1990Figure1Population)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1], 
#' 	m = Z_summary$m, 
#' 	y = Z_summary$y_total
#' )
#' @useDynLib ACSampling
#' @importFrom Rcpp sourceCpp
#' @export

var_x_HT <- function(N, n1, m, x, pi_i_values=NULL) {
	if (is.null(pi_i_values)) {
		pi_i_values 	<- pi_i(N, n1, m)
	}
	pi_ij_values 		<- pi_ij(N, n1, m) %>% as.matrix
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m), ncol=length(m), NA))
	# calculate for all pairs
	V <- var_x_HT_cpp(m, pi_i_values, pi_ij_values, x)
	sum(V, na.rm=T)/(N^2)
}