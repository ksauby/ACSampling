#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param MSE_BaselineSamplingDesign The sampling design to which the "comparison sampling design" is compared to and its efficiency, relative to this one, is calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @return Dataframe including original data and RE estimates.
#' @export

varybar <- function(x, N, sample.size.variable) {
	((N - sample.size.variable)*x) / (sample.size.variable*N)
}


RE <- function(x, N, sample.size.variable, variable_MSE) {
	varybar(x, N, sample.size.variable) / variable_MSE
}

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
			(length(population.grouping.variables) + 2):
			length(names(variances))
		], 
		paste(names(variances)[
				(length(population.grouping.variables) + 2):
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
	Y %<>% 
		merge(dimensions, by=population.grouping.variables) %>%
		merge(variances, by=population.grouping.variables) %>%
		group_by_(.dots=c(
			population.grouping.variables,
			paste(variables, "_mean_MSE", sep=""),
			"N",
			"sample.size.variable"
			)) %>%
		mutate_each(funs(
			varybar(., N=N, sample.size.variable=sample.size.variable)
		))
		setnames(
			Y,
			names(Y)[
				(length(c(
					population.grouping.variables,
					paste(variables, "_var", sep=""),
					"N",
					"sample.size.variable"
				)) + 1):
				length(names(Y))
			], 
			paste(
				variables,
				"varybar",
				sep="_"
			)
		)	
		
		
		group_by_(.dots=c(
			population.grouping.variables,
			paste(variables, "_mean_MSE", sep=""),
			"N",
			"sample.size.variable"
			)) %>%
		mutate_each(funs(
			varybar(., N=N, sample.size.variable=sample.size.variable)
		))
		
		
	for (i in 1:length(variables)) {
			
			
		}
		
			mutate(
				var_y_bar =  ((N - sample.size.variable)*population_variance) /
					(sample.size.variable*N),
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