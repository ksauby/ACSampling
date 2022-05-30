#' Create Summary of Cactus Realization Data
#' 
#' @param CactusRealizations the data to give the function
#' @return The data from Sauby and Christman.
#' 
#' @references 
#' \insertRef{saubyadaptive}{ACSampling}
#' @export

createCactusRealizationSummary <- function(CactusRealizations) {
     calcPopSummaryStats(
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