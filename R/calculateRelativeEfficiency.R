#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param MSE_BaselineSamplingDesign The sampling design to which the "comparison sampling design" is compared to and its efficiency, relative to this one, is calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @return Dataframe including original data and RE estimates.
#' @export

calculateRE <- function(
	MSE_ComparisonSamplingDesign,
	population_data,
	population.grouping.variables = population.grouping.variables,
	sample.size.variable = sample.size.variable,
	variables = variables
) {	
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (sample.size.variable %in% names(X)) {
		X %<>% setnames(sample.size.variable, "sample.size.variable")
	}
	# dimensions of populations
	dimensions <- population_data %>% 
		group_by_(.dots=population.grouping.variables) %>%
		summarise(N = n())
	# variances of populations
	variances <- population_data %>% 
		dplyr::select_(.dots=c(
			population.grouping.variables,
			variables
		)) %>%
		group_by_(.dots=population.grouping.variables) %>% 
		summarise_each(funs(var(., na.rm=T)))
	setnames(
		variances,
		names(variances)[
			(length(population.grouping.variables) + 1):
			length(names(variances))
		], 
		paste(names(variances)[
				(length(population.grouping.variables) + 1):
				length(names(variances))
			],
			"var",
			sep="_"
		)
	)	
	Y <- X %>% dplyr::select_(.dots=c(
		population.grouping.variables,
		"sample.size.variable",
		paste(variables, "_mean_MSE", sep=""
	)))
	# calculate var_y_bar
	Y %<>% 
		merge(dimensions, by=population.grouping.variables) %>%
		merge(variances, by=population.grouping.variables)
	A <- Y %>% 
		dplyr::select(-one_of(paste(variables, "_var", sep=""))) %>%
		reshape2:::melt.data.frame(
			data=.,
			id.vars=c(
				population.grouping.variables,
				"N",
				"sample.size.variable"
			),
			value.name="mean_MSE"
		)
	A$variable <- sub("*_mean_MSE", "", A$variable)
	B <- Y %>% 
		dplyr::select(-one_of(paste(variables, "_mean_MSE", sep=""))) %>%
		reshape2:::melt.data.frame(
			data=.,
			id.vars=c(
				population.grouping.variables,
				"N",
				"sample.size.variable"
			),
			value.name="population_variance"
		)	
	B$variable <- sub("*_var", "", B$variable)
	Z <- A %>%
		merge(
			B, 
			by=c(
				population.grouping.variables, 
				"N", 
				"sample.size.variable", 
				"variable"
			)
		)
	Z %<>% mutate(
		RE = (
			((N - sample.size.variable)*population_variance) /
			(sample.size.variable*N)
		)
		/	mean_MSE
	)
	Z %<>%
	dplyr::select_(.dots=c(
		population.grouping.variables, 
		"N", 
		"sample.size.variable", 
		"variable",
		"RE"
	))
	Z$variable <- paste(Z$variable, "_RE", sep="")
	Z %<>%
	reshape2::dcast(
		list(
			c(population.grouping.variables, "N", "sample.size.variable"),
			c("variable")
		),
		value.var="RE"
		) %>% head
	Z %<>% setnames("sample.size.variable", sample.size.variable)
	return(Z)
}