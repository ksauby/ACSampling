#' Calculate the number of units per network and the unit inclusion probability for each network FOR A SINGLE POPULATION
#' @description The purpose of this is to reduce computation time by calculating some necessary information before the data is bootstrapped. The function calculates (1) $pi_i$ (the unit inclusion probability) for each network and sample size (\code{nsamples}) and (2) the number of units per network for each of the \code{variables}.
#' @param popdata population data.
#' @param vars variables to summarise
#' @param groupvar categories that group networks
#' @param n1 Size(s) of the initial simple random sample(s) without replacement. Can be a single value or a vector values.
#' @param mvar The variable on which the ACS sampling criterion is based
#' @return The population data (one row per cell of each population), with additional columns indicating $pi_i$ for each network and (\code{n1}) and the number of units per network for each of the \code{vars}.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export
#' @importFrom dplyr funs summarise_all

summarizeNetworkInfo <- function(popdata, vars, groupvar=NULL, n1, mvar) {
	
     handleError_variable(vars, "vars")
     handleError_singlepopulation(N)
     
     
	N <- unique(popdata$N) #. <- NULL
	# calculate the number of units per network for each variable
	Networks <- popdata %>%
		# select these columns
		.[, c(variables, "NetworkID", groupvar, "m")] %>%
		# group_by these columns
		group_by_at(c("NetworkID", groupvar, "m")) %>%
		# calculate the sum of the remaining columns (the "variables" columns)
		summarise_all(funs(sum(., na.rm=T)))
	m <- paste("Networks$", m_var, sep="")
	# calculate pi_i for each network and sample size (nsample)
	for (i in 1:length(nsamples)) {
		Networks$Var = sapply(
			Networks$m,
			function(x) {pi_i(N=N, n1=nsamples[i], m=x)}
		)
		names(Networks)[dim(Networks)[2]] <- paste(
			"pi_i_n1_", nsamples[i], sep=""
		)
	}
	# 
	for (i in 1:length(variables)) {
		names(Networks) <- ifelse(
			names(Networks) %in% variables[i],
			paste(variables[i], "_network_sum", sep=""),
			names(Networks)
		)
	}
	popdata %<>% merge(Networks)
	return(popdata)
}