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
	population.grouping.variables,
	sample.size.variable,
	rvar,
	ovar
) {	
	X <- NULL
	X <- MSE_ComparisonSamplingDesign
	if (sample.size.variable %in% names(X)) {
		colnames(X)[names(X) == sample.size.variable)] <- "sample.size.variable"
	}
	# dimensions of populations
	dimensions <- population_data %>% 
		group_by_(.dots=population.grouping.variables) %>%
		summarise(N = n())
	# variances of populations
	if (!is.null(ovar)) {
		variances <- population_data %>% 
			dplyr::select_(.dots=c(
				population.grouping.variables,
				ovar,
				rvar
			)) %>%
			group_by_(.dots=population.grouping.variables) %>% 
			summarise_all(funs(var(., na.rm=T)))
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
	}
	if (!is.null(rvar)) {
		# should I only calculate this using samples that included at least one unit with cactus species?
		variances_ratio <- population_data %>% 
			filter(Stricta==1) %>%
			dplyr::select_(.dots=c(
				population.grouping.variables,
				rvar
			)) %>%
			group_by_(.dots=population.grouping.variables) %>% 
			summarise_all(funs(var(., na.rm=T)))
		setnames(
			variances_ratio,
			names(variances_ratio)[
				(length(population.grouping.variables) + 1):
				length(names(variances_ratio))
			], 
			paste(names(variances_ratio)[
					(length(population.grouping.variables) + 1):
					length(names(variances_ratio))
				],
				"ratio_var",
				sep="_"
			)
		)	
	}
	Y <- X %>% dplyr::select_(.dots=c(
		population.grouping.variables,
		"sample.size.variable",
		paste(ovar, "_mean_MSE", sep=""),
		paste(rvar, "_ratio_mean_MSE", sep="")		
	))
	# calculate var_y_bar
	if (!is.null(rvar) & is.null(ovar)) {
		Y %<>% 
			merge(dimensions, by=population.grouping.variables) %>%
			merge(variances_ratio, by=population.grouping.variables)
	}
	if (is.null(rvar) & !is.null(ovar)) {
		Y %<>% 
			merge(dimensions, by=population.grouping.variables) %>%
			merge(variances, by=population.grouping.variables)
	}
	if (!is.null(rvar) & !is.null(ovar)) {
		Y %<>% 
			merge(dimensions, by=population.grouping.variables) %>%
			merge(variances, by=population.grouping.variables) %>%
			merge(variances_ratio, by=population.grouping.variables)
	}
	# dataframe of mean MSE
	A <- Y %>% 
		dplyr::select(-one_of(paste(ovar, "_var", sep=""))) %>%
		dplyr::select(-one_of(paste(rvar, "_ratio_var", sep=""))) %>%
		dplyr::select(-one_of(paste(rvar, "_var", sep=""))) %>%
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
	# dataframe of y_var
	B <- Y %>% 
		dplyr::select(-one_of(
			paste(ovar, "_mean_MSE", sep=""),
			paste(rvar, "_ratio_mean_MSE", sep=""),		
			paste(rvar, "_var", sep="")		
		)) %>%
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
			population_variance/sample.size.variable *
			(1 - sample.size.variable/N)
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
		)
	colnames(Z)[names(Z) == "sample.size.variable")] <- sample.size.variable
	return(Z)
}