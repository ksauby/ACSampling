#' Calculate Simulation Data Sampling Bias
#' 
#' @param realization_data Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
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
	realization_data, 
	simulation_data, 
	grouping.variables, 
	variables, 
	rvar,
	statistics, 
	ratio.statistics,
	ACS=TRUE,
	roundn = 2
) 
{
	. <- NULL
	X <- merge(realization_data, simulation_data, by=grouping.variables)
	for (i in 1:length(variables)) {
		for (j in 1:length(statistics)) {
			X %<>%
				mutate(
					round(
						(
						#	(observed -
							eval(parse(text=paste("X$", variables[i], "_", 
								statistics[j], "_observed", sep=""))) - 
						#	true) / 
							eval(parse(text=paste("X$", variables[i], "_", 
								statistics[j], sep="")))
						) / 
						# true
						eval(parse(text=paste("X$", variables[i], "_", 
							statistics[j], sep=""))), roundn
					)*100
				) %>%
				setnames(., dim(.)[2], paste(variables[i], "_", statistics[j], 
					"_bias", sep=""))
			}
	}
	if (!(is.null(rvar))) {
		for (i in 1:length(rvar)) {
			for (j in 1:length(ratio.statistics)) {
				X %<>%
					mutate(
						round(
							(
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
								)))
							) / 
							# true
							eval(parse(text=paste(
								"X$", 
								rvar[i], 
								"_ratio_", 
								ratio.statistics[j], 
								sep=""
							))), roundn
						)*100
					
					) %>%
					setnames(., dim(.)[2], paste(rvar[i], "_", 
						ratio.statistics[j], "_bias", sep=""))
				}
		}
	}
	if (ACS==TRUE) {
		X$Prop.Area.Surveyed = round(with(X, N.Total.plots/N), roundn)		
	} else {
		X$Prop.Area.Surveyed = round(with(X, N.SRSWOR.plots/N), roundn)
	}
		#X$n.networks = unique(realization_data$n.networks)[h]
#	}
	return(X)
}