#' Calculate the Horvitz-Thompson mean of an adaptive cluster sample.
#' 
#' @template pi_i_values
#' @template N
#' @template n1
#' @template m_vec
#' @template y
#' @param sampling A vector (\code{character} format) describing whether units were included in the initial sample or subsequent ACS sample. Units selected in the initial sample should be given the value "Initial_Sample" in the \code{sampling} vector.
#' @template criterion
#' @description This calculate the Horvitz-Thompson mean of an adaptive cluster sample done by sampling without replacement.
#'
#'where \eqn{v} is the number of distinct units in the sample and
#'\eqn{J_k} is an indicator variable, equalling 0 if the \eqn{k^{th}} unit in the sample does not satisfy the condition and was not selected in the initial sample; otherwise, \eqn{J_k = 1}.
#' 
#' @return The Horvitz-Thompson mean.
#' @references 
#' \insertRef{thompson1990adaptive}{ACSampling}
#' @examples
#' library(magrittr)
#' library(plyr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # EXAMPLE 1: Sampling of population from Figure 1, Thompson (1990)
#'
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Figure1Sample)
#' 
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Figure1Sample, aes(x,y), shape=0, size=7)
#' 
#' Z <- createACS(popdata=Thompson1990Fig1Pop, 
#' n1=dim(Thompson1990Figure1Sample)[1], yvar="y_value", 
#' initsample=Thompson1990Figure1Sample)
#' # CALCULATE y_HT
#' y_HT(
#' 	N = dim(Thompson1990Fig1Pop)[1], 
#' 	n1 = dim(Thompson1990Figure1Sample)[1],
#' 	m_vec = Z$m, 
#' 	y = Z$y_value, 
#' 	sampling = Z$Sampling,
#' 	criterion=0
#' ) 
#'
#' # EXAMPLE 2: Table 1 from Thompson (1990)
#' data(Thompson1990Table1data)
#' (Thompson1990Table1 = Thompson1990Table1data %>%
#' group_by(sampling_effort) %>%
#' summarise(
#' 	`y (added through SRSWOR)` = toString(y_value[which(sampling=="SRSWOR")]),
#' 	`y (added through ACS)` = toString(y_value[which(sampling=="ACS")]),
#' 	y_bar_1 = mean(y_value[which(sampling=="SRSWOR")]),
#' 	y_HT = round(y_HT(N=dim(Thompson1990Fig1Pop)[1], n1, m, y_value, sampling, 5), 2),
#' 	y_bar = round(mean(y_value),2)
#' 	)
#' )
#' 
#' # EXAMPLE 3: 
#' # data(cactus_realizations)
#' # realization = cactus_realizations %>% filter(n.networks==40)

#' @template ex_Thompson2002_2_p_307_values
#' @template ex_Thompson2002_2_p_307_y_HT
#' @export

y_HT <- function(y, N, n1, pi_i_values=NULL, m_vec=NULL, sampling=NULL, criterion=NULL) {
	if (!(is.null(sampling)) & !(is.null(criterion))) {
		J = ifelse(y >= criterion | sampling=="Initial_Sample", 1, 0)
	} else {
		J = 1
	}
	if (is.null(pi_i_values)) {
		pi_i_values = pi_i(N, n1, m_vec)
	}
	y_HT = sum(y*J/pi_i_values, na.rm=T)/N
	return(y_HT)	
}





#' Calculate the Horvitz-Thompson mean of an adaptive cluster sample, NEW FORMULA.
#' 
#' @template pi_i_values
#' @template N
#' @template n1
#' @template m_vec
#' @template y
#' @param sampling A vector (\code{character} format) describing whether units were included in the initial sample or subsequent ACS sample. Units selected in the initial sample should be given the value "Initial_Sample" in the \code{sampling} vector.
#' @template criterion
#' @template m_threshold
#' @description This calculate the Horvitz-Thompson mean of an adaptive cluster sample done by sampling without replacement.
#'
#'where $v$ is the number of distinct units in the sample and
#'$J_k$ is an indicator variable, equalling 0 if the $k$ th unit in the sample does not satisfy the condition and was not selected in the initial sample; otherwise, $J_k = 1$.
#' 
#' @return The Horvitz-Thompson mean.
#' @references 
#' \insertRef{saubyadaptive}{ACSampling}
#' \insertRef{thompson1990adaptive}{ACSampling}
#'
#' @export
#' @examples @template ex_Thompson2002_2_p_307_values
#' @template ex_Thompson2002_2_p_307_y_HT

new_y_HT <- function(y, N, n1, m_threshold, pi_i_values=NULL, m_vec=NULL, sampling=NULL, criterion=NULL) {
	if (!(is.null(sampling)) & !(is.null(criterion))) {
		J = ifelse(y >= criterion | sampling=="Initial_Sample", 1, 0)
	} else {
		J = 1
	}
	Z = data.frame(y=y, m_vec=m_vec)
	
	# with replacement inclusion probability for cluster units
	# pi_i = 1 - ( 1 - ((m + a)/N) )^ n
	# how to make this work for a network
	
	A <- Z %>% filter(m_vec <= m_threshold)
	B <- Z %>% filter(m_vec > m_threshold)
	if (dim(A)[1] > 0) {
		A$pi_i_values = pi_i(N, n1, A$m_vec)	
	}
	if (dim(B)[1] > 0) {	
		B$pi_i_values = pi_i(N, n1, m_threshold)
	}
	Z <- bind_rows(A, B) %>% as.data.frame
	y_HT = sum(unlist(Z$y)*J/unlist(Z$pi_i_values), na.rm=T)/N
	return(y_HT)	
}
