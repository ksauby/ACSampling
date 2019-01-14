calculatedSquaredDifferences <- function(dataframe, variables) {
	# for each simulation and variable, calculate:
	#	(observed mean - true mean)^2
	for (i in 1:length(variables)) {
		dataframe %<>%
			mutate(
				observed_minus_true_squared = 
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
				"observed_minus_true_squared", 
				paste(
					variables[i], 
					"_mean_observed_minus_true_squared", 
					sep=""
				)
			)
	}
	return(dataframe)
}
calculateDifferencesinMeans <- function(dataframe, variables) {
	# for each simulation and variable, calculate:
	#	observed mean - mean of observed means
	for (i in 1:length(variables)) {
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

#' @importFrom dplyr summarise_ n
#' @importFrom lazyeval interp

calculateBiasComponents	<- function(dataframe, resultslist, variables) {
	for (i in 1:length(variables)) {
		resultslist[[i]] <- list()
		resultslist[[i]] <- dataframe %>%
			summarise_(
				# save true mean
				mean_RB_true_mean = interp(
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
				mean_RB_mean_of_observed_means = interp(
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
				# sample size used to calculate mean of observed means
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
				# calculate MSE
				MSE_sum_of_observed_minus_true_sqrd = interp(
					~sum(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed_minus_true_squared",
							sep=""
						)
					)
				),
				# sample size used to calculate MSE
				MSE_n_sims = interp(
					~length(var[which(!is.na(var))]), 
					var = 
					as.name(
						paste(
							variables[i],
							"_mean_observed_minus_true_squared",
							sep=""
						)
					)
				),
				# for var_RB: calculate mean of simulated HT variance estimates
				var_RB_mean_of_var_estimates = interp(
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
				# for var_RB: sample size used to calculate mean of simulated HT variance estimates
				var_RB_mean_of_var_estimates_n = interp(
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
				# for var_RB: calculate variance of _mean_observed
				var_RB_var_of_mean_estimates = interp(
					~var(variable, na.rm = TRUE), 
					variable = 
					as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				# for var_RB: calculate sample size used to calculate variance of _mean_observed
				var_RB_var_of_mean_estimates_n = interp(
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
			as.data.frame
		resultslist[[i]] %<>% 
			mutate(
				RB = 100 * 
					(mean_RB_mean_of_observed_means - mean_RB_true_mean) / 
					mean_RB_true_mean,
					
				MSE = MSE_sum_of_observed_minus_true_sqrd/MSE_n_sims,
				
				var_RB = 100 *
				(var_RB_mean_of_var_estimates - var_RB_var_of_mean_estimates) / 
					var_RB_var_of_mean_estimates,
					
				var_RB_n = min(
					var_RB_mean_of_var_estimates_n, 
					var_RB_var_of_mean_estimates_n
				)
			) %>%
			setnames(
	      		.,
	      		"mean_RB_true_mean",
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
	      		"MSE_n_sims",
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
				mean_RB_mean_of_observed_means,
				MSE_sum_of_observed_minus_true_sqrd,
				var_RB_mean_of_var_estimates,
				var_RB_mean_of_var_estimates_n,
				var_RB_var_of_mean_estimates,
				var_RB_var_of_mean_estimates_n
			))
	}
	return(resultslist)
}
calculateMeanofObservedMeans <- function(dataframe, variables, nsims) {
	# group data to prep for processing by 
	#		population.grouping.variables, 
	#		sampling.grouping.variables, and
	#		n_sims
	temp <- dataframe %>% group_by_(.dots = c(
		population.grouping.variables, 
		sampling.grouping.variables, 
		"n_sims"
	))
	# list for saving results
	C <- vector("list", length(variables))
	# for each of the population.grouping.variables and sampling.grouping.variables, calculate:
	#	mean of observed means
	#	sample size used for those calculations
	for (i in 1:length(variables)) {
		C[[i]] <- list()
		# observed mean
		C[[i]] <- temp %>%
			summarise_(
				mean_of_observed_means = interp(
					~mean(var, na.rm = TRUE), 
					var = as.name(
						paste(
							variables[i],
							"_mean_observed",
							sep=""
						)
					)
				),
				sample_size = interp(
					~length(var[which(!is.na(var))]), 
					var = as.name(
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
		      	"mean_of_observed_means",
		      	paste(
		      		variables[i],
		      		"_mean_of_observed_means",
		      		sep=""
				)
			) %>%
			setnames(
		      	.,
		      	"sample_size",
		      	paste(
		      		variables[i],
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
	dataframe %<>% merge(
		D, 
		by=c(
			population.grouping.variables, 
			sampling.grouping.variables,
			"n_sims"
		)
	)
	return(dataframe)
}
#' Calculate Simulation Data Sampling Bias
#' 
#' @param population_data_summary Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
#' @param simulation_data Simulation data on sampling of the multiple patch realizations.
#' @param population.grouping.variables Categorical variables with which to group the population data (e.g., artificial population number if there are more than 1)
#' @param sampling.grouping.variables Categorical variables with which to group the simulation data (e.g., sampling design used, number of primary samples).
#' @param ovar Vector of variables for which sampling bias should be estimated.
#' @param o_rvar Vector of variables for which secondary variables should be estimated. Can be identical to ovar or a subset.
#' @param rvar Variables for which to use ratio estimators

#' @description

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
	ovar,
	o_rvar,
	rvar
)
{
	rvar_variables <- paste(rvar, "_ratio", sep="")
	. <- n_sims <- A <- B <- C <- D <- E <- I <- G <- H <- NULL
	A <- merge(
		population_data_summary, 
		simulation_data, 
		by=population.grouping.variables
	)
	# ------------------------------------------------------------------------ #
	# OCCUPANCY VARIABLES	
	# ------------------------------------------------------------------------ #
	B <- A 
	# add number of simulations to dataset
	B %<>% 
		group_by_(.dots = c(
			population.grouping.variables, 
			sampling.grouping.variables
		)) %>%
		mutate(n_sims = n())
	
	B %<>% calculateMeanofObservedMeans(dataframe=B, variables=ovar)
	
	# WHY IS IT ONLY CALC MEAN OF OBS MEANS FOR RVAR?
	
	
	
	
	
	
	
	# ------------------------------------------------------------------------ #
	# RATIO VARIABLES	
	# ------------------------------------------------------------------------ #
	E <- A
	# E %<>% filter(Stricta_mean_observed!=0)
	
	E %<>% calculateMeanofObservedMeans(dataframe=B, variables=c(rvar_variables))
	
	
	E %<>% calculatedSquaredDifferences(variables=c(o_rvar, rvar_variables))
	E %<>% calculateDifferencesinMeans(variables=c(o_rvar, rvar_variables))

	B %<>% calculatedSquaredDifferences(variables=ovar)
	B %<>% calculateDifferencesinMeans(variables=ovar)
	
	# calculate bias and MSE
	# occupancy variables
	H <- vector("list", length(ovar))
	X.grp <- B %>% 
		group_by_(.dots=c(
			population.grouping.variables, 
			sampling.grouping.variables, 
			"n_sims"
		)) %>% 
		calculateBiasComponents(., resultslist=H, variables=ovar)
	
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
	) %>%
		setnames("n_sims", "ovar_n_sims")
	# ratio variables
	H <- vector("list", length(rvar))
	
	
	
	

	
	
	X.grp <- E %>% 
		group_by_(.dots=c(
			population.grouping.variables, 
			sampling.grouping.variables, 
			"n_sims"
		)) %>% 
		calculateBiasComponents(., resultslist=H, variables=rvar_variables)
	Z <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				population.grouping.variables, 
				sampling.grouping.variables, 
				"n_sims"
			)
		),
		X.grp
	) %>%
		setnames("n_sims", "rvar_n_sims")
	Y %<>% merge(Z, 
		by=c(
			population.grouping.variables, 
			sampling.grouping.variables
		),
		all=T
	)
	
	return(Y)
}