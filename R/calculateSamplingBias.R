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



















calculatealternateSamplingBias <- function(
	realization_data, 
	simulation_data, 
	grouping.variables, 
	variables, 
	rvar,
	statistics, 
	ratio.statistics,
	ACS=TRUE,
	roundn = 6
) 
{
	. <- NULL
	X <- merge(realization_data, simulation_data, by=grouping.variables)
	A <- vector("list", length(variables))
	X.grp <- X %>% group_by_(.dots=c(grouping.variables, "N.SRSWOR.plots"))
	for (i in 1:length(variables)) {
		A[[i]] <- list()
		A[[i]] <- X.grp %>%
			summarise_(
				# calculate sample mean
				var_samplemean = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(
						paste(
							variables[i],
							"_", 
							statistics[1], 
							"_observed",
							sep=""
						)
					)
				),
				# save true mean
				var_mean = interp(
					~var[1], 
					var = 
					as.name(
						paste(
							variables[i],
							"_", 
							statistics[1], 
							sep=""
						)
					)
				)
			) 
		A[[i]] %<>% mutate(
			# calculate bias
			var_bias = var_samplemean - var_mean,
			# calculate relative bias
			var_relativebias = var_bias/var_mean
		)
		# CALCULATE VARIANCE OF THE HT MEAN
		temp <- X %>% merge(A[[i]], by=c(grouping.variables, "N.SRSWOR.plots"))
		temp$var_varHT_i <- (
			# HT mean
			eval(
				parse(
					text=paste(
						"temp$",
						variables[i],
						"_", 
						statistics[1], 
						"_observed",
						sep=""
					)
				)
			) - 
			# mean of the HT means 
			temp$var_samplemean	
		)^2
		temp %<>%
			group_by_(.dots=c(grouping.variables, "N.SRSWOR.plots")) %>%
			summarise(var_varHT = sum(var_varHT_i)/length(var_varHT_i))	
		A[[i]] %<>% merge(temp, by=c(grouping.variables, "N.SRSWOR.plots"))
		# CALCULATE MSE OF THE HT MEAN
		A[[i]] %<>% mutate(var_MSE = var_bias^2 + var_varHT) %>%
		# CHANGES VARIABLE NAMES
		setnames(
			.,
			"var_mean",
			paste(
				variables[i],
				"_",
				statistics[1], 
				sep=""
			)
		) %>%
		setnames(
			.,
			"var_samplemean",
			paste(
				variables[i],
				"_",
				statistics[1], 
				"_samplemean",
				sep=""
			)
		) %>%
		setnames(
			.,
			"var_bias",
			paste(
				variables[i],
				"_",
				statistics[1], 
				"_bias",
				sep=""
			)
		) %>%
		setnames(
			.,
			"var_relativebias",
			paste(
				variables[i],
				"_",
				statistics[1], 
				"_relativebias",
				sep=""
			)
		) %>%
		setnames(
			.,
			"var_varHT",
			paste(
				variables[i],
				"_",
				statistics[1], 
				"_varHT",
				sep=""
			)
		) %>%
   		setnames(
   			.,
   			"var_MSE",
   			paste(
   				variables[i],
   				"_",
   				statistics[1], 
   				"_MSE",
   				sep=""
   			)
   		)
	}
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(grouping.variables, "N.SRSWOR.plots")
		),
		A
	)
	nums <- sapply(Y, is.numeric)
	Y[, nums] %<>% round(roundn)
	summ <- X.grp %>%
		summarise(
			# other summary variables
			Restricted = Restricted[1],
			m_min = m_min[1],
			m_max = m_max[1],
			m_mean = m_mean[1],
			m_var = m_var[1],
			n_Species_Patches = n_Species_Patches[1],
			N = N[1],
			Plots = Plots[1]
		) 
	Y %<>% merge(summ)
	return(Y)
}
	
	
	
	
	
	
	