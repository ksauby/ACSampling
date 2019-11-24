#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param popgroupvar Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}). WHAT ELSE
#' @param popdata Dataframe of population data.
#' @param samplesizevar Name of column in population data (?) containing the variable indicating variation in sample size.
#' @param rvar Ratio variables.
#' @param ovar Occupancy variables.

#' @description Calculate efficiency of sampling design, relative to WHAT.

#' @return Dataframe including original data and RE estimates.

#' @export
#' @importFrom reshape2 dcast
#' @importFrom utils getFromNamespace

# dimensions of populations
# dimensions <- popdata %>% 
#	group_by_(.dots=popgroupvar) %>%
#	summarise(N = n())
# variances of populations




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



simulation_data_summary_table_re = calculateSamplingBias(
	population_data_summary	= patch_data_summary_wide, 
	simulation_data		= simdata_all_re, 
	sampgroupvar	= sampgroupvar, 
	popgroupvar = popgroupvar,
	ovar			= ovar, 
	rvar				= rvar
)


RE_values <- calculateRE(
	population_data = patch_data_summary_wide,
	MSE_ComparisonSamplingDesign = simulation_data_summary_table_re,
	popgroupvar = "population",
	sample.size.variable = "N.Total.plots_mean",
	ovar = ovar,
	rvar = rvar
)

calculateRE <- function(
	MSE_ComparisonSamplingDesign,
	popdata,
	popgroupvar,
	samplesizevar,
	rvar,
	ovar
) {	
	melt.data.frame = getFromNamespace("melt.data.frame", "reshape2")
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (samplesizevar %in% names(X)) {
		colnames(X)[names(X) == samplesizevar] <- "samplesizevar"
	}
	# "long" format of mean MSE
	A <- X %>% select_(.dots=c(
		popgroupvar,
		"samplesizevar",
		paste(ovar, "_mean_MSE", sep=""),
		paste(rvar, "_ratio_mean_MSE", sep="")		
	)) %>%
	melt.data.frame(
		data=.,
		id.vars=c(
			popgroupvar,
			"samplesizevar"
		),
		value.name="mean_MSE"
	)
	A$variable <- sub("*_mean_MSE", "", A$variable)
	# "long" format of population variance
	B <- popdata %>% 
		select_(.dots=c(
			popgroupvar,
			paste(ovar, "_var", sep=""),
			paste(rvar, "_ratio_var", sep=""),
			"N"
		)) %>%			
		melt.data.frame(
			data=.,
			id.vars=c(
				popgroupvar,
				"N"
			),
			value.name="population_variance"
		)	
	B$variable <- sub("*_var", "", B$variable)
	# merge together
	Z <- A %>%
		merge(
			B, 
			by=c(
				popgroupvar, 
				"variable"
			)
		)
	# calculate efficiency
	Z %<>% mutate(
		RE = (
			.data$population_variance/samplesizevar *
			(1 - samplesizevar/.data$N)
		)
		/	.data$mean_MSE
	)
	Z$variable <- paste(Z$variable, "_RE", sep="")
	Z %<>%
	dcast(
		list(
			c(popgroupvar, "N", "samplesizevar"),
			c("variable")
		),
		value.var="RE"
		)
	colnames(Z)[names(Z) == "samplesizevar"] <- samplesizevar
	return(Z)
}