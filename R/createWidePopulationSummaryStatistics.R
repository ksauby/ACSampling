#' Create "Wide" Format Population Summary Statistics for Population Data
#' 
#' @param popsummarystats Created by the function calculatePopSummaryStats.
#' @param ovar vector of occupancy variables
#' @param rvar vector of ratio variables
#' @return Dataframe with a column per mean/variance of each variable and a row per population.
#' @export
CactusRealizationSummary <- calculatePopSummaryStats(
	popdata = CactusRealizations, 
	summaryvar = c("Stricta", "Pusilla", "Cactus",
		"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
		"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
	popgroupvar = "population", 
	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", 
		"Percent_Cover_Stricta", "Height_Stricta", 
		"Old_Moth_Evidence_Stricta"),
	nrow=30,
	ncol=30
)
patch_data_summary_wide <- createWidePopSummaryStats(
	popsummarystats = CactusRealizationSummary,
	ovar = "Stricta",
	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
		"Height_Stricta", "Old_Moth_Evidence_Stricta")
)


createWidePopSummaryStats <- function(popsummarystats, ovar, rvar) {
	# variance
	popsummarystats[[2]]$variable %<>% as.character()
	A <- popsummarystats[[2]] %>%
		select(.data$Var, .data$variable, .data$population) %>%
		mutate(
			variable = ifelse(
				.data$variable %in% rvar,
				paste(.data$variable, "ratio", sep="_"),
				.data$variable
			)
		) %>%
		spread(key=variable, value=Var)
	nvar <- length(unique(popsummarystats[[2]]$variable))
	names(A)[2:(nvar + 1)] <- paste(names(A)[2:(nvar + 1)], "var", sep="_")
	# mean
	B <- popsummarystats[[2]] %>%
		select(.data$Mean, .data$variable, .data$population) %>%
		mutate(
			variable = ifelse(
				.data$variable %in% rvar,
				paste(.data$variable, "ratio", sep="_"),
				.data$variable
			)
		) %>%
		spread(key=variable, value=Mean)
	names(B)[2:(nvar + 1)] <- paste(names(B)[2:(nvar + 1)], "mean", sep="_")
	# population size
	C <- popsummarystats[[1]] %>%
		select(.data$population, .data$N)
	# merge all together
	merge(A, B, by="population") %>% merge(C, by="population")
}