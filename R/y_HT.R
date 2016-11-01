#' Calculate the Horvitz-Thompson mean of an adaptive cluster sample.
#' 
#' @param pi_i_values vector of inclusion probabilities, if not calculated using this function. Default is \code{NULL}.
#' @param N Population size. 
#' @param m Number of units satisfying the ACS criterion in network $i$.
#' @param n1 Initial sample size.
#' @param y Attribute data about species of interest (e.g., abundance, presence/absence).
#' @param sampling A vector (\code{character} format) describing whether units were included in the initial sample or subsequent ACS sample. Units selected in the initial sample should be given the value "Initial_Sample" in the \code{sampling} vector.
#' @param criterion The threshold value of \code{y} that triggers adaptive cluster sampling.
#' @description This calculate the Horvitz-Thompson mean of an adaptive cluster sample done by sampling without replacement.
#'
#'where $v$ is the number of distinct units in the sample and
#'$J_k$ is an indicator variable, equalling 0 if the $k$ th unit in the sample does not satisfy the condition and was not selected in the initial sample; otherwise, $J_k = 1$.
#' 
#' @return The Horvitz-Thompson mean.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @examples 
#' library(magrittr)
#' library(plyr)
#' library(dplyr)
#' library(ggplot2)
#'
#' # EXAMPLE 1: Sampling of population from Figure 1, Thompson (1990)
#'
#' data(Thompson1990Figure1Population)
#' data(Thompson1990Figure1Sample)
#' 
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Figure1Population, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Figure1Sample, aes(x,y), shape=0, size=7)
#' 
#' # REPLACE WITH CREATEACS FUNCTION
#'
#' # INITIATE ACS
#' # assign species information to units in the initial sample
#' S = merge(
#' 	Thompson1990Figure1Population,
#' 	Thompson1990Figure1Sample, 
#' 	all.y=TRUE
#' )
#' 
# create list of neighboring ("cluster") plots
#' Z = list()  					
#' S$Sampling <- "Initial_Sample"
#' # add the rest of the units for each network in the initial sample
#' Z = rbind.fill(S, Thompson1990Figure1Population %>% 
#' 	filter(Thompson1990Figure1Population$NetworkID %in% S$NetworkID))
#' Z[which(is.na(Z$Sampling)), ]$Sampling <- "Cluster"
#' Networks = filter(Z, y_value > 0)
#' # fill in edge units
#' E = as.data.frame(cbind(
#' 	x = rowSums(expand.grid(Networks$x, c(1,-1,0,0))),
#' 	y = rowSums(expand.grid(Networks$y, c(0,0,1,-1)))
#' )) %>% 
#' mutate(Sampling="Edge")
#' # remove duplicate units
#' Z %<>% rbind.fill(E) %>%
#' mutate(temp_coords = paste(x, y, sep=""))
#' Z =  Z[!duplicated(Z$temp_coords),]
#' Z %<>% dplyr::select(-temp_coords)
#' # fill in y_value
#' Z[which(is.na(Z$y_value)), ]$y_value <- 0
#' # fill in m
#' Z[which(Z$y_value==0 & Z$Sampling=="Edge"), ]$m <- 0
#' 
#' 	N = dim(Thompson1990Figure1Population)[1] 
#' 	n1 = dim(Thompson1990Figure1Sample)[1]
#' 	m = Z$m
#' 	y = Z$y_value
#' 	sampling = Z$Sampling
#' 	criterion=0

#' # CALCULATE y_HT
#' y_HT(
#' 	N = N, 
#' 	n1 = n1,
#' 	m = m, 
#' 	y = y, 
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
#' 	y_HT = round(y_HT(N, n1, m, y_value, sampling, 5), 2),
#' 	y_bar = round(mean(y_value),2)
#' 	)
#' )
#' 
#' # EXAMPLE 3: 
#' # data(cactus_realizations)
#' # realization = cactus_realizations %>% filter(n.networks==40)

#' # EXAMPLE 4:
#' # Ch. 24, Exercise #2, p. 307, from Thompson (2002)
#' # Horvitz-Thompson mean times the population size; should equal 38
#' y_HT(
#'     N 		= 1000, 
#'     n1 		= 100, 
#'     m 		= c(2,3,rep(1,98)), 
#'     y 		= c(3,6,rep(0, 98)),
#'     sampling = "SRSWOR",
#'     criterion =0
#' )*1000 %>% round(0)

#' @export

y_HT <- function(y, N, n1, pi_i_values=NULL, m=NULL, sampling=NULL, criterion=NULL) {
	if (!(is.null(sampling)) & !(is.null(criterion))) {
		J = ifelse(y >= criterion | sampling=="SRSWOR", 1, 0)
	} else {
		J = 1
	}
	if (is.null(pi_i_values)) {
		pi_i_values = pi_i(N, n1, m)
	}
	y_HT = sum(y*J/pi_i_values, na.rm=T)/N
	return(y_HT)	
}