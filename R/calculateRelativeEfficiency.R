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
	sampling.grouping.variables = sampling.grouping.variables,
	sample.size.variable = sample.size.variable,
	variables = variables
) {	
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (sample.size.variable %in% names(X)) {
		X %<>% setnames(sample.size.variable, "sample.size.variable")
	}
	for (i in 1:length(variables)) {
		variances <- population_data %>% 
			group_by_(.dots=population.grouping.variables) %>% 
			summarise_(
				population_variance = interp(
					~var(variable, na.rm = TRUE), 
					variable = 
					as.name(variables[i])
				),
				N = N[1]
			)
		X %<>% 
			merge(variances, by=population.grouping.variables) %>%
			mutate(
				var_y_bar =  population_variance / sample.size.variable * (1 - sample.size.variable/N),
				RE = var_y_bar /
				# comparison sampling design
					eval(
						parse(
							text=paste(
								"X$",
								variables[i], 
								"_mean_MSE", 
								sep=""
							)
						)
					)
			) %>%
			setnames(
				., 
				"RE", 
				paste(
					variables[i], 
					"_mean_RE", 
					sep=""
				)
			) %>%
			dplyr::select(-c(var_y_bar,population_variance,N))
	}
	X %<>% setnames("sample.size.variable", sample.size.variable)
	return(X)
}