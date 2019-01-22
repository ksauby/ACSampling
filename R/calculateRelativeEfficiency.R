#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param population.grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}). WHAT ELSE
#' @param population_data Dataframe of population data.
#' @param sample.size.variable Name of column in population data (?) containing the variable indicating variation in sample size.
#' @param rvar Ratio variables.
#' @param ovar Occupancy variables.

#' @description Calculate efficiency of sampling design, relative to WHAT.

#' @return Dataframe including original data and RE estimates.

#' @export
#' @importFrom reshape2 dcast
#' @importFrom utils getFromNamespace

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
	melt.data.frame = getFromNamespace("melt.data.frame", "reshape2")
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (sample.size.variable %in% names(X)) {
		colnames(X)[names(X) == sample.size.variable] <- "sample.size.variable"
	}
	# "long" format of mean MSE
	A <- X %>% select_(.dots=c(
		population.grouping.variables,
		"sample.size.variable",
		paste(ovar, "_mean_MSE", sep=""),
		paste(rvar, "_ratio_mean_MSE", sep="")		
	)) %>%
	melt.data.frame(
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
		select_(.dots=c(
			population.grouping.variables,
			paste(ovar, "_var", sep=""),
			paste(rvar, "_ratio_var", sep=""),
			"N"
		)) %>%			
		melt.data.frame(
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
			.data$population_variance/sample.size.variable *
			(1 - sample.size.variable/.data$N)
		)
		/	.data$mean_MSE
	)
	Z$variable <- paste(Z$variable, "_RE", sep="")
	Z %<>%
	dcast(
		list(
			c(population.grouping.variables, "N", "sample.size.variable"),
			c("variable")
		),
		value.var="RE"
		)
	colnames(Z)[names(Z) == "sample.size.variable"] <- sample.size.variable
	return(Z)
}