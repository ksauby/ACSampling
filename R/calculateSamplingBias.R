#' Calculate Simulation Data Sampling Bias
#' 
#' @param population_data_summary Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
#' @param simulation_data Simulation data on sampling of the multiple patch realizations.
#' @param population.grouping.variables Categorical variables with which to group the population data (e.g., artificial population number if there are more than 1)
#' @param sampling.grouping.variables Categorical variables with which to group the simulation data (e.g., sampling design used, number of primary samples).
#' @param ovar Vector of variables for which sampling bias should be estimated.
#' @param rvar Variables for which to use ratio estimators
#' @return Dataframe including simulation data summary statistics, including relative bias and mean squared error (MSE) of the mean and variance.
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
	rvar
)
{
	
	# limit MSE of ratio variables to samples including at least one unit with cacti
	
	# have to know correct number of simulations
		
	
	
	rvar_variables <- paste(rvar, "_ratio", sep="")
	. <- NULL
	A <- merge(
		population_data_summary, 
		simulation_data, 
		by=population.grouping.variables
	)
	### OCCUPANCY VARIABLES	
	B <- A %>% dplyr::select_(.dots=c(
		population.grouping.variables, 
		sampling.grouping.variables,
		paste(
			ovar,
			"_mean",
			sep=""
		),
		paste(
			ovar,
			"_mean_observed",
			sep=""
		)
	))
	# number of simulations
	n_sims <- B %>% 
		group_by_(.dots = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)) %>%
		summarise(n_sims = n()
		)
	B %<>% merge(
		n_sims, 
		by = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)
	)
	# mean of observed means	
	temp <- B %>% group_by_(.dots = c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	))
	C <- vector("list", length(ovar))
	for (i in 1:length(ovar)) {
		C[[i]] <- list()
		# observed mean
		C[[i]] <- temp %>%
			summarise_(
				mean_of_observed_means = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							ovar[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				sample_size = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							ovar[i],
							"_mean_observed",
							sep=""
						)
					)
				)
			) %>%
			setnames(
		      	.,
		      	"mean_of_observed_means",
		      	paste(
		      		ovar[i],
		      		"_mean_of_observed_means",
		      		sep=""
				)
			) %>%
			setnames(
		      	.,
		      	"sample_size",
		      	paste(
		      		ovar[i],
		      		"_mean_of_observed_means_n",
		      		sep=""
				)
			)
	}
	D <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		C
	)
	B %<>% merge(
		D, 
		by=c(
			population.grouping.variables, 
			sampling.grouping.variables,
			"n_sims"
		)
	)
	### RATIO VARIABLES	
	E <- A %>% dplyr::select_(.dots=c(
		population.grouping.variables, 
		sampling.grouping.variables,
		paste(
			rvar_variables,
			"_mean",
			sep=""
		),
		paste(
			rvar_variables,
			"_mean_observed",
			sep=""
		),
		"Stricta_mean_observed"
	))
	# number of simulations
	n_sims <- E %>% 
		filter(Stricta_mean_observed>0) %>%
		group_by_(.dots = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)) %>%
		summarise(n_sims = n()
		)
	E %<>% merge(
		n_sims, 
		by = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)
	)
	# mean of observed means	
	temp <- E %>% group_by_(.dots = c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	))
	F <- vector("list", length(ovar))
	for (i in 1:length(ovar)) {
		F[[i]] <- list()
		# observed mean
		F[[i]] <- temp %>%
			summarise_(
				mean_of_observed_means = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							rvar_variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				sample_size = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							rvar_variables[i],
							"_mean_observed",
							sep=""
						)
					)
				)
			) %>%
			setnames(
		      	.,
		      	"mean_of_observed_means",
		      	paste(
		      		rvar_variables[i],
		      		"_mean_of_observed_means",
		      		sep=""
				)
			) %>%
			setnames(
		      	.,
		      	"sample_size",
		      	paste(
		      		rvar_variables[i],
		      		"_mean_of_observed_means_n",
		      		sep=""
				)
			)
	}
	G <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		F
	)
	E %<>% merge(
		G, 
		by=c(
			population.grouping.variables, 
			sampling.grouping.variables,
			"n_sims"
		)
	)
	B %<>% calculatedSquaredDifferences(variables=ovar)
	E %<>% calculatedSquaredDifferences(variables=paste(rvar, "_ratio", sep=""))
	B %<>% calculateDifferencesinMeans(variables=ovar)
	E %<>% calculateDifferencesinMeans(variables=paste(rvar, "_ratio", sep=""))
	
	
	
	
	
	
	
	calculatedSquaredDifferences <- function(dataframe, variables) {
		# calculate squared differences for each simulation and variable
		for (i in 1:length(ovar)) {
			dataframe %<>%
				mutate(
					observed_minus_true = 
							# observed -
							(eval(
								parse(
									text=paste(
										variables[i], 
										"_mean_observed", 
										sep=""
									)
								)
							) - 
							# true
							eval(
								parse(
									text = paste(
									variables[i], 
									"_mean", 
									sep=""
								)
							)
							)
					)^2
				) %>%
				setnames(
					., 
					"observed_minus_true", 
					paste(
						variables[i], 
						"_mean_observed_minus_true", 
						sep=""
					)
				)
		}
		return(dataframe)
	}
	calculateDifferencesinMeans <- function(dataframe, variables) {
		for (i in 1:length(ovar)) {
			dataframe %<>%
				mutate(
					observed_minus_mean_of_observed_means = 
							# (observed -
							eval(
								parse(
									text = paste(
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
										variables[i], 
										"_mean_of_observed_means", 
										sep=""
									)
								)
							)
				) %>%
				setnames(
					., 
					"observed_minus_mean_of_observed_means", 
					paste(
						variables[i], 
						"_observed_minus_mean_of_observed_means", 
						sep=""
					)
				)
		}
		return(dataframe)
	}
	calculateBiasComponents	<- function(dataframe, resultslist, variables) {
		for (i in 1:length(ovar)) {
			resultslist[[i]] <- list()
			resultslist[[i]] <- dataframe %>%
				summarise_(
					# save true mean
					true_mean = interp(
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
					mean_of_observed_means = interp(
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
					RB_n = interp(
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
					sum_of_observed_minus_true_sqrd = interp(
						~sum(var, na.rm = TRUE), 
						var = 
						as.name(
							paste(
								variables[i],
								"_mean_observed_minus_true",
								sep=""
							)
						)
					),
					MSE_n = interp(
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
					mean_of_var_estimates = interp(
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
					mean_of_var_estimates_n = interp(
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
					var_of_mean_estimates = interp(
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
					var_of_mean_estimates_n = interp(
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
			resultslist[[i]] %<>% 
				mutate(
					# var_mean = Mean()
					RB = 100 * 
						(mean_of_observed_means - true_mean) / 
						true_mean,
					MSE = sum_of_observed_minus_true_sqrd/n_sims,
					var_RB = 100 * (mean_of_var_estimates - 
						var_of_mean_estimates) / 
						var_of_mean_estimates,
					var_RB_n = min(
						mean_of_var_estimates_n, 
						var_of_mean_estimates_n
					)
				) %>%
				setnames(
		      		.,
		      		"true_mean",
		      		paste(
		      			variables[i],
		      			"_true_mean",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"RB",
		      		paste(
		      			variables[i],
		      			"_mean_RB",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"MSE",
		      		paste(
		      			variables[i],
		      			"_mean_MSE",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_RB",
		      		paste(
		      			variables[i],
		      			"_var_RB",
		      			sep=""
					)
				) %>%
				# rename sample size variables
				setnames(
		      		.,
		      		"RB_n",
		      		paste(
		      			variables[i],
		      			"_mean_RB_n",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"MSE_n",
		      		paste(
		      			variables[i],
		      			"_mean_MSE_n",
		      			sep=""
					)
				) %>%
				setnames(
		      		.,
		      		"var_RB_n",
		      		paste(
		      			variables[i],
		      			"_var_RB_n",
		      			sep=""
					)
				) %>%
				dplyr::select(-c(
					mean_of_observed_means,
					sum_of_observed_minus_true_sqrd,
					mean_of_var_estimates,
					mean_of_var_estimates_n,
					var_of_mean_estimates,
					var_of_mean_estimates_n
				))
		}
		return(resultslist)
	}
	# calculate bias and MSE
	H <- vector("list", length(ovar))
	X.grp <- B %>% group_by_(.dots=c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	)) %>% calculateBiasComponents(., resultslist=H, variables=ovar)
	
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		X.grp
	)
	
#	X$Prop.Area.Surveyed = with(X, N.Total.plots/N)		
	return(Y)
}