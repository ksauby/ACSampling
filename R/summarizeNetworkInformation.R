#' Calculate the number of units per network and the unit inclusion probability for each network FOR A SINGLE POPULATION
#' @description The purpose of this is to reduce computation time by calculating some necessary information before the data is bootstrapped. The function calculates (1) $pi_i$ (the unit inclusion probability) for each unit, given the size of its associated network and sample size (\code{n1_vec}) and (2) for each of the \code{variables}, the sum of the values of that variable for each network.
#' @param popdata population data.
#' @param vars variables to summarise
#' @template popvar_default_null
#' @template n1_vec
#' @template yvar
#' @return The population data (one row per cell of each population), with additional columns indicating $pi_i$ for each network and (\code{n1}) and the number of units per network for each of the \code{vars}. WHERE the value of the var is greater than zero?
#' @references 
#' \insertRef{saubyadaptive}{ACSampling}
#' @export
#' @importFrom dplyr funs summarise_all

summarizeNetworkInfo <- function(popdata, vars, popvar=NULL, n1_vec, yvar) {
	
     handleError_variable(vars, "vars")
     
	N <- unique(popdata$N) #. <- NULL 
	# handleError_singlepopulation(N)
     
	# calculate the number of units per network for each variable
	Networks <- popdata %>%
		# select these columns
		.[, c(vars, "NetworkID", popvar, "m")] %>%
		# group_by these columns
		group_by_at(c("NetworkID", popvar, "m")) %>%
		# calculate the sum of the remaining columns (the "vars" columns)
		summarise_all(list(~sum(., na.rm=T)))
	m <- paste("Networks$", yvar, sep="")
	# calculate pi_i for each network and sample size
	for (i in 1:length(n1_vec)) {
		Networks$Var = sapply(
			Networks$m,
			function(x) {pi_i(N=N, n1=n1_vec[i], m_vec=x)}
		)
		names(Networks)[dim(Networks)[2]] <- paste(
			"pi_i_n1_", n1_vec[i], sep=""
		)
	}
	# 
	for (i in 1:length(vars)) {
		names(Networks) <- ifelse(
			names(Networks) %in% vars[i],
     			paste(vars[i], "_network_sum", sep=""),
			names(Networks)
		)
	}
	popdata %<>% merge(Networks) %>%
	     arrange(NetworkID)
	return(popdata)
}