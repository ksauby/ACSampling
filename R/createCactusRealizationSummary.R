#' Create Summary of Cactus Realization Data
#' 
#' @param CactusRealizations the data to give the function
#' @return The data from Sauby and Christman.
#' 
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

createCactusRealizationSummary <- function(CactusRealizations) {
	calculatePopulationSummaryStatistics(
		popdata = CactusRealizations, 
		summaryvar = c("Stricta", "Pusilla", "Cactus",
			"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
		popgroupvar = "population", 
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", 
			"Percent_Cover_Stricta", "Height_Stricta", 
			"Old_Moth_Evidence_Stricta")
	)
}