#' Calculate Simulation Data Sampling Bias
#' 
#' @param population_data_summary Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
#' @param simulation_data Simulation data on sampling of the multiple patch realizations.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @param variables Vector of variables for which sampling bias should be estimated.
#' @param statistics The statistics that should be used to estimate the bias.
#' @param ACS Adaptive cluster sampling? Defaults to \code{TRUE}.
#' @param rvar Variables for which to use ratio estimators
#' @param ratio.statistics Statistics (e.g., mean and variance) to calculate using the ratio estimators
#' @param roundn Number of decimal points to retain when calculating bias
#' @return Dataframe including simulation data summary statistics, including sampling bias (difference between true population parameter and the sampling estimate).
#' @examples
#' # Create realizations
#' x_start = 1
#' x_end = 30
#' y_start = 1
#' y_end = 30
#' n.networks = c(5, 15, 10, 20, 30, 40)
#' n.realizations = 1
#' SpeciesInfo = PlotSurveys_season1
#' Species.Fields = c("Stricta", "Pusilla", "Cactus")
#' cactus.realizations = createSpeciesPatchRealizations(x_start, x_end,
#' 	y_start, y_end, buffer=5, n.networks, n.realizations, SpeciesInfo, 
#' 	start.seed=1, Species.Fields)
#' 
#' # Sample from the realizations
#' simulations=1
#' nsamples=c(5,10,20,40)
#' population <- createPopulation(x_start = 1, x_end = 30, y_start = 1, 
#' 	y_end = 30)
#' abundance.variables = NULL
#' occupancy.variables = c(
#' 	"Stricta",
#' 	"Pusilla",
#' 	"Cactus",
#' 	"CACA_on_Pusilla",
#' 	"CACA_on_Stricta",
#' 	"MEPR_on_Pusilla",
#' 	"MEPR_on_Stricta",
#' 	"Old_Moth_Evidence_Pusilla",
#' 	"Old_Moth_Evidence_Stricta"
#' 	# "Percent_Cover_Pusilla", # how do I do these? they are occupancy nor abundance
#' 	# "Percent_Cover_Stricta",
#' 	# "Height_Pusilla",
#' 	# "Height_Stricta",
#' )		
#' patch_data = cactus.realizations
#' # simulation_data <- sampleSpeciesPatchRealizations(patch_data, simulations, 
#' #	nsamples, population, abundance.variables, occupancy.variables)
#' 
#' # summary.variables = occupancy.variables
#' # grouping.variables = c("n.networks", "realization")
#' # dataset = cactus.realizations
#' # patch_data_summary <- calculateRealizationSummaryStatistics(dataset, 
#' # 	summary.variables, grouping.variables)
#' # avar = NULL
#' @export

calculateSamplingBias <- function(
	population_data_summary, 
	simulation_data, 
	population.grouping.variables, 
	sampling.grouping.variables,
	variables, 
	rvar,
	statistics, 
	ratio.statistics,
	ACS=TRUE
) 
{
	variables <- c(variables, rvar)
	. <- NULL
	X <- merge(population_data_summary, simulation_data, by=population.grouping.variables)
	# number of simulations
	n_sims <- X %>% 
		group_by_(.dots = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)) %>%
		summarise(n_sims = length(N.Total.plots))
	X <- merge(
		X, 
		n_sims, 
		by = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)
	)
	# mean of observed means	
	temp <- X %>% group_by_(.dots = c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	))
	A <- vector("list", length(variables))
	for (i in 1:length(variables)) {
		A[[i]] <- list()
		A[[i]] <- temp %>%
			summarise_(
				var_mean_of_observed_means = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				var_n = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				)
			) %>%
			setnames(
		      	.,
		      	"var_mean_of_observed_means",
		      	paste(
		      		variables[i],
		      		"_mean_of_observed_means",
		      		sep=""
				)
			) %>%
			setnames(
		      	.,
		      	"var_n",
		      	paste(
		      		variables[i],
		      		"_mean_of_observed_means_n",
		      		sep=""
				)
			)
	}
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		A
	)
	X <- merge(
		X, 
		Y, 
		by=c(
			population.grouping.variables, 
			sampling.grouping.variables,
			"n_sims"
		)
	)
	# calculate squared differences for each simulation and variable
	for (i in 1:length(variables)) {
		for (j in 1:length(statistics)) {
			X %<>%
				mutate(
					var_observed_minus_true = 
							# (observed -
							eval(
								parse(
									text=paste(
										"X$", 
										variables[i], 
										"_", 
										statistics[j], 
										"_observed", 
										sep=""
									)
								)
							) - 
							# true)
							eval(
								parse(
									text=paste(
										"X$", 
										variables[i], 
										"_", 
										statistics[j], 
										sep=""
									)
								)
							)
				) %>%
				setnames(
					., 
					"var_observed_minus_true", 
					paste(
						variables[i], 
						"_", 
						statistics[j], 
						"_observed_minus_true", 
						sep=""
					)
				)
		}
	}
	# calculate squared differences (observed - mean of observed) for each variable
	for (i in 1:length(variables)) {
		X %<>%
			mutate(
				var_observed_minus_mean_of_observed_means = 
						# (observed -
						eval(
							parse(
								text = paste(
									"X$", 
									variables[i], 
									"_mean_observed", 
									sep=""
								)
							)
						) - 
						# true)
						eval(
							parse(
								text = paste(
									"X$", 
									variables[i], 
									"_mean_of_observed_means", 
									sep=""
								)
							)
						)
			) %>%
			setnames(
				., 
				"var_observed_minus_mean_of_observed_means", 
				paste(
					variables[i], 
					"_observed_minus_mean_of_observed_means", 
					sep=""
				)
			)
	}
	# calculate bias and MSE
	A <- vector("list", length(variables))
	X.grp <- X %>% group_by_(.dots=c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	))
	for (i in 1:length(variables)) {
		A[[i]] <- list()
		A[[i]] <- X.grp %>%
			summarise_(
				# save true mean
				var_true_mean = interp(
					~var[1], 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean", 
							sep=""
						)
					)
				),
				# calculate mean of observed means
				var_mean_of_observed_means = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				var_RB_n = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				# calculate sum of MSEs
				var_sum_of_observed_minus_true_sqrd = interp(
					~sum(var^2, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed_minus_true",
							sep=""
						)
					)
				),
				var_MSE_n = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed_minus_true",
							sep=""
						)
					)
				),
				# calculate mean of simulated HT variance estimates
				var_mean_of_var_estimates = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_var_observed",
							sep=""
						)
					)
				),
				var_mean_of_var_estimates_n = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_var_observed",
							sep=""
						)
					)
				),
				# calculate variance of simulated HT mean estimates
				var_var_of_mean_estimates = interp(
					~var(variable, na.rm = TRUE), 
					variable = 
					as.name(
						paste(
							variables[i],
							"_observed_minus_mean_of_observed_means",
							sep=""
						)
					)
				),
				var_var_of_mean_estimates_n = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_observed_minus_mean_of_observed_means",
							sep=""
						)
					)
				)
			) %>%
				as.data.frame
			A[[i]] %<>% 
				mutate(
					# var_mean = Mean()
					var_RB = 100 * 
						(var_mean_of_observed_means - var_true_mean) / 
						var_true_mean,
					var_MSE = var_sum_of_observed_minus_true_sqrd/n_sims,
					var_var_RB = 100 * (var_mean_of_var_estimates - 
						var_var_of_mean_estimates) / 
						var_var_of_mean_estimates,
					var_var_RB_n = min(
						var_mean_of_var_estimates_n, 
						var_var_of_mean_estimates_n
					)
				) %>%
				setnames(
		      		.,
		      		"var_true_mean",
		      		paste(
		      			variables[i],
		      			"_true_mean",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_RB",
		      		paste(
		      			variables[i],
		      			"_mean_RB",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_MSE",
		      		paste(
		      			variables[i],
		      			"_mean_MSE",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_var_RB",
		      		paste(
		      			variables[i],
		      			"_var_RB",
		      			sep=""
					)
				) %>%
				# rename sample size variables
				setnames(
		      		.,
		      		"var_RB_n",
		      		paste(
		      			variables[i],
		      			"_mean_RB_n",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_MSE_n",
		      		paste(
		      			variables[i],
		      			"_mean_MSE_n",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_var_RB_n",
		      		paste(
		      			variables[i],
		      			"_var_RB_n",
		      			sep=""
					)
				) %>%
				dplyr::select(-c(
					var_mean_of_observed_means,
					var_sum_of_observed_minus_true_sqrd,
					var_mean_of_var_estimates,
					var_mean_of_var_estimates_n,
					var_var_of_mean_estimates,
					var_var_of_mean_estimates_n
				))
	}
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		A
	)
#	X$Prop.Area.Surveyed = with(X, N.Total.plots/N)		
	return(Y)
}
	
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