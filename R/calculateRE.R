#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @template popvar
#' @param popdata Dataframe of population data.
#' @template samplesizevar
#' @template rvar
#' @template ovar

#' @description Calculate efficiency of sampling design, relative to WHAT.

#' @return Dataframe including original data and RE estimates.

#' @export
#' @importFrom reshape2 dcast
#' @importFrom utils getFromNamespace
#' @importFrom tidyr pivot_longer
#' @examples
#'# dimensions of populations
#'#  library(magrittr)
#'#  library(dplyr)
#' # WHERE IS POP DATA FROM?
#' # dimensions <- popdata %>% 
#'# 	group_by(popvar) %>%
#'#	summarise(N = n())
#' # variances of populations
#'# CactusRealizationSummary <- calcPopSummaryStats(
#'# 	# popdata = CactusRealizations, 
#'# 	# summaryvar = c("Stricta", "Pusilla", "Cactus",
#'# 		"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#'# 		"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
#'# 	popvar = "Island", 
#'# 	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", 
#'# 		"Percent_Cover_Stricta", "Height_Stricta", 
#'# 		"Old_Moth_Evidence_Stricta"),
#'# 	nrow=30,
#'# 	ncol=30
#'# )
#'# patch_data_summary_wide <- createWidePopSummaryStats(
#'# 	popsummarystats = CactusRealizationSummary,
#'# 	ovar = "Stricta",
#'# 	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#'# 		"Height_Stricta", "Old_Moth_Evidence_Stricta")
#'# )



#'# simulation_data_summary_table_re = calcSamplingBias(
#'# 	population_data_summary	= patch_data_summary_wide, 
#'# 	simulation_data		= simdata_all_re, 
#'# 	sampgroupvar	= sampgroupvar, 
#'# 	popvar = popvar,
#'# 	ovar			= ovar, 
#'# 	rvar				= rvar
#'# )


#'# RE_values <- calcRE(
#'# 	population_data = patch_data_summary_wide,
#'# 	MSE_ComparisonSamplingDesign = simulation_data_summary_table_re,
#'# 	popvar = "population",
#'# 	samplesizevar = "N.Total.plots_mean",
#'# 	ovar = ovar,
#'# 	rvar = rvar
#'# )

calcRE <- function(
	MSE_ComparisonSamplingDesign,
	popdata,
	popvar,
	samplesizevar,
	rvar,
	ovar
) {	

	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (samplesizevar %in% names(X)) {
		colnames(X)[names(X) == samplesizevar] <- "samplesizevar"
	}
	# "long" format of mean MSE
	orvar <- c(
		popvar,
		"samplesizevar",
		paste(ovar, "_mean_MSE", sep=""),
		paste(rvar, "_ratio_mean_MSE", sep="")		
		)
	ORVAR <- syms(orvar)
     A <- X %>% 
          select(!!!ORVAR) %>%
          pivot_longer(.,
               -c(popvar, "samplesizevar"),
               names_to = "variable",
               values_to = "mean_MSE"
          )
	A$variable <- sub("*_mean_MSE", "", A$variable)
	# "long" format of population variance
	poporvar <- c(
          popvar,
          paste(ovar, "_var", sep=""),
          paste(rvar, "_ratio_var", sep=""),
          "N"
	)
	POPORVAR <- syms(poporvar)
	B <- popdata %>% 
		select(!!!POPORVAR) %>%			
		pivot_longer(.,
			-c(popvar, "N"),
			names_to="variable",
			values_to="population_variance"
		)	
	B$variable <- sub("*_var", "", B$variable)
	# merge together
	Z <- A %>% merge(B, by=c(popvar, "variable"))
	# calculate efficiency
	Z %<>% mutate(
		RE = (
			.data$population_variance/samplesizevar *
			(1 - samplesizevar/.data$N)
		)
		/	.data$mean_MSE
	)
	Z$variable <- paste(Z$variable, "_RE", sep = "")
	Z %<>%
	dcast(
		list(
			c(popvar, "N", "samplesizevar"),
			c("variable")
		),
		value.var="RE"
		)
	colnames(Z)[names(Z) == "samplesizevar"] <- samplesizevar
	return(Z)
}