#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param MSE_BaselineSamplingDesign The sampling design to which the "comparison sampling design" is compared to and its efficiency, relative to this one, is calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).

#' @description

#' @return Dataframe including original data and RE estimates.

#' @export


# dimensions of populations
# dimensions <- population_data %>% 
#	group_by_(.dots=population.grouping.variables) %>%
#	summarise(N = n())
# variances of populations

calculateRE <- function(
	MSE_ComparisonSamplingDesign,
	population_data,
	population.grouping.variables,
	sample.size.variable,
	rvar,
	ovar
) {	
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (sample.size.variable %in% names(X)) {
		colnames(X)[names(X) == sample.size.variable] <- "sample.size.variable"
	}
	# "long" format of mean MSE
	A <- X %>% dplyr::select_(.dots=c(
		population.grouping.variables,
		"sample.size.variable",
		paste(ovar, "_mean_MSE", sep=""),
		paste(rvar, "_ratio_mean_MSE", sep="")		
	)) %>%
	reshape2:::melt.data.frame(
		data=.,
		id.vars=c(
			population.grouping.variables,
			"sample.size.variable"
		),
		value.name="mean_MSE"
	)
	A$variable <- sub("*_mean_MSE", "", A$variable)
	# "long" format of population variance
	B <- population_data %>% 
		dplyr::select_(.dots=c(
			population.grouping.variables,
			paste(ovar, "_var", sep=""),
			paste(rvar, "_ratio_var", sep=""),
			"N"
		)) %>%			
		reshape2:::melt.data.frame(
			data=.,
			id.vars=c(
				population.grouping.variables,
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
				population.grouping.variables, 
				"variable"
			)
		)
	# calculate efficiency
	Z %<>% mutate(
		RE = (
			population_variance/sample.size.variable *
			(1 - sample.size.variable/N)
		)
		/	mean_MSE
	)
	Z$variable <- paste(Z$variable, "_RE", sep="")
	Z %<>%
	reshape2::dcast(
		list(
			c(population.grouping.variables, "N", "sample.size.variable"),
			c("variable")
		),
		value.var="RE"
		)
	colnames(Z)[names(Z) == "sample.size.variable"] <- sample.size.variable
	return(Z)
}