#' Calculate Relative Efficiency (RE)
#' 
#' @param MSE_ComparisonSamplingDesign Sampling design for which relative efficiency (RE) should be calculated.
#' @param MSE_BaselineSamplingDesign The sampling design to which the "comparison sampling design" is compared to and its efficiency, relative to this one, is calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @return Dataframe including original data and RE estimates.
#' @export


calculateRE <- function(
	MSE_ComparisonSamplingDesign = MSE_ComparisonSamplingDesign,
	MSE_BaselineSamplingDesign = MSE_BaselineSamplingDesign,
	grouping.variables = grouping.variables,
	variables = variables
) {	
	X <- merge(
		MSE_BaselineSamplingDesign, 
		MSE_ComparisonSamplingDesign, 
		by=grouping.variables
	)	
	for (i in 1:length(variables)) {
		X %<>%
			mutate(
				# baseline sampling design
				RE = eval(
					parse(
						text=paste(
							"X$",
							variables[i], 
							"_mean_MSE.x", 
							sep=""
						)
					)
				) /
				# comparison sampling design
				eval(
					parse(
						text=paste(
							"X$",
							variables[i], 
							"_mean_MSE.y", 
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
			dplyr::select_(.dots=setdiff(names(.), c(
				paste(
					variables[i], 
					"_mean_MSE.x", 
					sep=""
				),
				paste(
					variables[i], 
					"_mean_MSE.y", 
					sep=""
				)
			)))			
	}
	return(X)
}