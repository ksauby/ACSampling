#' Create "Wide" Format Population Summary Statistics for Patch Population Data
#' 
#' @param PopulationSummaryStatistics Created by the function calculatePopulationSummaryStatistics.
#' @param ovar list of occupancy variables
#' @param rvar list of ratio variables
#' @return Dataframe with a column per mean/variance of each variable and a row per population.
#' @export


createWidePopSummaryStats <- function(PopulationSummaryStatistics, ovar, rvar) {
	# variance
	A <- PopulationSummaryStatistics[[2]] %>%
		select(.data$Var, .data$variable, .data$population) %>%
		rowwise() %>%
		mutate(
			variable = replace(
				.data$variable,
				which(.data$variable %in% rvar),
				paste(.data$variable, "ratio", sep="_")
			)
		) %>%
		ungroup() %>%
		reshape2::dcast(., population ~ variable, value.var="Var")
	nvar <- length(unique(PopulationSummaryStatistics[[2]]$variable))
	names(A)[2:(nvar + 1)] <- paste(names(A)[2:(nvar + 1)], "var", sep="_")
	# mean
	B <- PopulationSummaryStatistics[[2]] %>%
		select(.data$Mean, .data$variable, .data$population) %>%
		rowwise() %>%
		mutate(
			variable = replace(
				.data$variable,
				which(.data$variable %in% rvar),
				paste(.data$variable, "ratio", sep="_")
				)
		) %>%
		ungroup() %>%
		reshape2::dcast(., population ~ variable, value.var="Mean")
	names(B)[2:(nvar + 1)] <- paste(names(B)[2:(nvar + 1)], "mean", sep="_")
	# population size
	C <- PopulationSummaryStatistics[[1]] %>%
		select(.data$population, .data$N)
	# merge all together
	merge(A, B, by="population") %>% merge(C, by="population")
}