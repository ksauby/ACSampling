#' Calculate the number of units per network and the unit inclusion probability for each network. 
#' @description The purpose of this is to reduce computation time by calculating some necessary information before the data is boostrapped. The function calculates (1) $pi_i$ (the unit inclusion probability) for each network and sample size (\code{nsamples}) and (2) the number of units per network for each of the \code{variables}.
#' @param population_data population data
#' @param variables variables to summarise
#' @param grouping.variables categories that group networks
#' @param nsamples Size(s) of the initial simple random sample(s) without replacement. Can be a single value or a vector values.
#' @param m_var The variable on which the ACS sampling criterion is based
#' @return The population data (one row per cell of each population), with additional columns indicating $pi_i$ for each network and (\code{nsamples}) and the number of units per network for each of the \code{variables}.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export
#' @importFrom dplyr funs summarise_all

summarizeNetworkInformation <- function(population_data, variables, grouping.variables=NULL, nsamples, m_var) {
	stopifnot(length(unique(patch_data$N)) == 1)
	N <- unique(patch_data$N) #. <- NULL
	# calculate the number of units per network for each variable
	Networks <- patch_data %>%
		# select these columns
		.[, c(variables, "NetworkID", grouping.variables, "m")] %>%
		# group_by these columns
		group_by_(.dots=c("NetworkID", grouping.variables, "m")) %>%
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
	patch_data %<>% merge(Networks)
	return(patch_data)
}