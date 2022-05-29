#' Calculate the variance of the Horvitz-Thompson estimator of the mean
#' @template pi_i_values
#' @template N
#' @template n1
#' @template m_vec
#' @template y_total
#'
#' @template Thompson1990
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
#' Z = createACS(popdata=Thompson1990Fig1Pop, 
#' 	n1=dim(Thompson1990Figure1Sample)[1], 
#' 	initsample=Thompson1990Figure1Sample, yvar="y_value")
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


var_y_HT <- function(N, n1, m_vec, y_total, pi_i_values=NULL) {
	if (is.null(pi_i_values)) {
		pi_i_values 	<- pi_i(N, n1, m_vec)
	}
	pi_ij_values 		<- pi_ij(N, n1, m_vec) %>% as.matrix
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m_vec), ncol=length(m_vec), NA))
	# calculate for all pairs
	V <- var_y_HT_cpp(m_vec, pi_i_values, pi_ij_values, y_total)
	sum(V, na.rm=T)/(N^2)
}


#' Calculate the variance of the Horvitz-Thompson estimator of the mean using the RACS correction
#' @template N
#' @template n1
#' @template m_vec
#' @template y_total
#' @template pi_i_values
#' @template m_threshold
#' @template SaubyCitation
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
#' Z = createACS(
#' popdata = Thompson1990Fig1Pop, 
#' n1 = dim(Thompson1990Figure1Sample)[1], 
#' initsample = Thompson1990Figure1Sample, 
#' yvar = "y_value"
#' )
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
#' 	y_total = Z_summary$y_total,
#' 	m_threshold=3
#' )

var_y_HT_RACS <- function(N, n1, m_vec, y_total, m_threshold, pi_i_values=NULL) {
	Z = data.frame(y_total=y_total, m_vec=m_vec) %>% arrange(m_vec)
	A <- Z %>% filter(m_vec <= m_threshold)
	B <- Z %>% filter(m_vec > m_threshold)
	if (dim(A)[1] > 0) {
		A$pi_i_values = pi_i(N, n1, A$m_vec)	
	}
	if (dim(B)[1] > 0) {	
		B$pi_i_values = pi_i(N, n1, m_threshold)
	}
	Z <- bind_rows(A, B) %>% as.data.frame
	pi_i_values 		<- Z$pi_i_values
	pi_ij_values 		<- pi_ij_RACS(N, n1, m_vec, m_threshold) %>% as.matrix
	# replace diagonal (where h = j)
	diag(pi_ij_values) 	<- Z$pi_i_values	
	# dataframe to store sum(k=1 to kappa) sum(m=1 to kappa)
	V = as.data.frame(matrix(nrow=length(m_vec), ncol=length(m_vec), NA))
	# calculate for all pairs
	V <- var_y_HT_cpp(m_vec, pi_i_values, pi_ij_values, y_total)
	sum(V, na.rm=T)/(N^2)
}
