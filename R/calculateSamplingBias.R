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
	grouping.variables, 
	variables, 
	rvar,
	statistics, 
	ratio.statistics,
	ACS=TRUE
) 
{
	. <- NULL
	X <- merge(population_data_summary, simulation_data, by=grouping.variables)
	for (i in 1:length(variables)) {
		for (j in 1:length(statistics)) {
			X %<>%
				mutate(
					var_observed_minus_true = 
					#	(observed -
						eval(parse(text=paste("X$", variables[i], "_", 
							statistics[j], "_observed", sep=""))) - 
					#	true) / 
						eval(parse(text=paste("X$", variables[i], "_", 
							statistics[j], sep=""))),
					var_bias = (
						var_observed_minus_true / 
						# true
						eval(parse(text=paste("X$", variables[i], "_", 
							statistics[j], sep="")))
					)*100,
					var_MSE_i = var_observed_minus_true^2
				) %>%
				setnames(., "var_bias", paste(variables[i], "_", statistics[j], 
					"_bias", sep="")) %>%
				setnames(., "var_MSE_i", paste(variables[i], "_", statistics[j], 
					"_MSE_i", sep=""))
					
			}
	}
	if (!(is.null(rvar))) {
		for (i in 1:length(rvar)) {
			for (j in 1:length(ratio.statistics)) {
				X %<>%
					mutate(
						var_observed_minus_true = 
							# (observed -
							eval(parse(text=paste(
								"X$", 
								rvar[i],
							 	"_", 
								ratio.statistics[j], 
								"_observed", 
								sep=""
							))) - 
							# true) / 
							eval(parse(text=paste(
								"X$", 
								rvar[i], 
								"_ratio_", 
								ratio.statistics[j], 
								sep=""
							))),
						var_bias = 
							(
								var_observed_minus_true / 
								# true
								eval(parse(text=paste(
									"X$", 
									rvar[i], 
									"_ratio_", 
									ratio.statistics[j], 
									sep=""
								)))
							)*100,
						var_MSE_i = var_observed_minus_true^2
					) %>%
					setnames(., "var_bias", paste(rvar[i], "_", 
						ratio.statistics[j], "_bias", sep="")) %>%
					setnames(., "var_MSE_i", paste(rvar[i], "_", 
						ratio.statistics[j], "_MSE_i", sep=""))
				}
		}
	}
	if (ACS==TRUE) {
		X$Prop.Area.Surveyed = with(X, N.Total.plots/N)		
	} else {
		X$Prop.Area.Surveyed = with(X, N.SRSWOR.plots/N)
	}
		#X$n.networks = unique(population_data_summary$n.networks)[h]
#	}
	X %<>% dplyr::select(-var_observed_minus_true)
	return(X)
}






#' Calculate Mean Squared Error
#' 
#' @param simulation_data_summary Calculate Mean Squared Error (MSE).
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @param variables Vector of variables for which MSE should be estimated.
#' @param rvar Variables for which to use ratio estimators
#' @param statistics Statistics (e.g., mean and variance) for which to calculate MSE
#' @return Dataframe including original data and MSE estimates.
#' @export

calculateMSE <- function(
	simulation_data_summary, 
	grouping.variables, 
	variables, 
	rvar,
	statistics
) 
{
	. <- NULL
	variables %<>% c(rvar)
	A <- vector("list", length(variables))
	X <- simulation_data_summary 
	n_sims <- X %>% 
		group_by_(.dots=c(grouping.variables)) %>%
		summarise(n_sims = length(N.Total.plots))
	X <- merge(X, n_sims, by=c(grouping.variables))
	X.grp <- X %>% group_by_(.dots=c(grouping.variables, "n_sims"))
	for (i in 1:length(variables)) {
		A[[i]] <- list()
		A[[i]] <- X.grp %>%
			summarise_(
				# calculate sum of MSEs
				var_MSE_sum = interp(
					~sum(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_", 
							statistics[1], 
							"_MSE_i",
							sep=""
						)
					)
				)
			) %>%
			as.data.frame
		A[[i]] %<>% 
			mutate(var_MSE_total = var_MSE_sum/n_sims) %>%
			setnames(
	      		.,
	      		"var_MSE_total",
	      		paste(
	      			variables[i],
	      			"_",
	      			statistics[1], 
	      			"_MSE",
	      			sep=""
				)
			) %>%
			dplyr::select(-c(var_MSE_sum))
	}	
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(grouping.variables, "n_sims")
		),
		A
	)				
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
	MSE_BaselineSamplingDesign = MSE_SRSWOR,
	grouping.variables = grouping.variables
) {	
	X <- merge(
		MSE_SRSWOR, 
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
			)			
	}
	return(X)
}