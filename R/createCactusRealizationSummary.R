#' Create Summary of Cactus Realization Data
#' 
#' @return The data from Sauby and Christman.
#' 
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

createCactusRealizationSummary <- function(CactusRealizations) {
	calculatePopulationSummaryStatistics(
		population_data = CactusRealizations, 
		summary.variables = c("Stricta", "Pusilla", "Cactus",
			"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
		population.grouping.variable = "population", 
		ratio.variables = c("MEPR_on_Stricta", "CACA_on_Stricta", 
			"Percent_Cover_Stricta", "Height_Stricta", 
			"Old_Moth_Evidence_Stricta")
	)
}