#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param MSE_BaselineSamplingDesign The sampling design to which the "comparison sampling design" is compared to and its efficiency, relative to this one, is calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @return Dataframe including original data and RE estimates.
#' @export


calculateRE <- function(
	MSE_ComparisonSamplingDesign = MSE_ComparisonSamplingDesign,
	population_data = patch_data,
	population.grouping.variables = population.grouping.variables,
	sampling.grouping.variables = sampling.grouping.variables,
	variables = variables
) {	
	X <- MSE_ComparisonSamplingDesign
	for (i in 1:length(variables)) {
		variances <- population_data %>% 
			group_by_(.dots=population.grouping.variables) %>% 
			summarise_(
				population_variance = interp(
					~var(variable, na.rm = TRUE), 
					variable = 
					as.name(variables[i])
				)
			)
		X %<>% 
			merge(variances, by=population.grouping.variables) %>%
			group_by_(.dots=c(
				population.grouping.variables, 
				sampling.grouping.variables
			)) %>% 
			summarise(
				var_y_bar = population_variance[1]/N.Total.plots_mean[1]
			) %>%
			merge(
				X,
				by=c(
					population.grouping.variables, 
					sampling.grouping.variables
				)
			) %>%
			mutate(
				RE = var_y_bar /
				# comparison sampling design
				eval(
					parse(
						text=paste(
							"temp$",
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
			dplyr::select(-var_y_bar)
	}
	return(X)
}