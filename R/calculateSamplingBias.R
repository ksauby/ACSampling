## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

calcSqDiff <- function(dataframe, Vars) {
	# for each simulation and Vars, calculate:
	#	(observed mean - true mean)^2
	for (i in 1:length(Vars)) {
		cname = paste(Vars[i], "MeanObs_True2", sep="")
		dataframe %<>%
			mutate(
				Obs_True2 = (
					# observed -
					eval(parse(text=paste(Vars[i],"MeanObs",sep=""))) - 
					# true
					eval(parse(text = paste(Vars[i], "TrueMean", sep="")))
				)^2
			) %>%
			rename(!!cname := Obs_True2)
	}
	return(dataframe)
}
calcDiffMeans <- function(dataframe, Vars) {
	# for each simulation and variable, calculate:
	#	observed mean - mean of observed means
	for (i in 1:length(Vars)) {
		cname = paste(Vars[i], "_Obs_MeanObsMeans", sep="")
		dataframe %<>%
			mutate(
				Obs_MeanObsMeans = 
					# (observed -
					eval(parse(text = paste(Vars[i], "MeanObs", sep="")))- 
					# true)
					eval(parse(text = paste(Vars[i], "_MeanObsMeans",sep="")))
			) %>%
			setnames(!!cname := Obs_MeanObsMeans)
	}
	return(dataframe)
}

#' @importFrom lazyeval interp
#' @importFrom rlang .data

calculateBiasComponents	<- function(dataframe, resultslist, Vars) {
	for (i in 1:length(Vars)) {
		resultslist[[i]] <- list()
		VAR = sym(paste(Vars[i],"TrueMean", sep=""))		
		var_observed = sym(paste(Vars[i],"MeanObs", sep=""))
		varMeanObs_True2 = sym(paste(Vars[i],"MeanObs_True2",sep=""))
		varVarObs = sym(paste(Vars[i],"VarObs",sep=""))
		resultslist[[i]] <- dataframe %>%
			summarise(
				# save true mean
				mean_RB_true_mean = var(!!VAR)[1],
				MeanRBMeanOfObsMean = mean(!!var_observed, na.rm=TRUE),
				RB_n = length(!is.na(!!var_observed)),
				MSESumObs_True2 = sum(!!VarMeanObsMinusTrue2, na.rm = TRUE), 
				MSESampleSize = length(!is.na(!!VarMeanObsMinusTrue2)),
				# for var_RB: calculate mean of simulated HT variance estimates
				varRBMeanOfVarEst = mean(!!varVarObs, na.rm = TRUE), 
				# for var_RB: sample size used to calculate mean of simulated HT variance estimates
				varRBMeanOfVarEst_n = length(!is.na(!!varVarObs)),
				# for var_RB: calculate variance of MeanObs
				varRBVarOfMeanEst = var(!!var_observed, na.rm = TRUE),
				# for var_RB: calculate sample size used to calculate variance of MeanObs
				varRBVarOfMeanEst_n = length(!is.na(!!var_observed))
			)
		resultslist[[i]] %<>% 
			mutate(
				RB = 100 * 
					(.data$MeanRBMeanOfObsMean - .data$mean_RB_true_mean) / 
					.data$mean_RB_true_mean,
				MSE = 
					.data$MSESumObs_True2/
					.data$MSESampleSize,
				var_RB = 100 *
					(.data$varRBMeanOfVarEst - .data$varRBVarOfMeanEst) / 
					.data$varRBVarOfMeanEst,
				var_RB_n = min(
					.data$varRBMeanOfVarEst_n, 
					.data$varRBVarOfMeanEst_n
				)
			)
		cnames <- c(
      		paste(Vars[i], "TrueMean", sep=""),
      		paste(Vars[i], "MeanRB", sep=""),
      		paste(Vars[i], "MeanMSE", sep=""),
      		paste(Vars[i], "VarRB", sep=""),
			# rename sample size Vars
			paste(Vars[i], "MeanRBn", sep=""),
      		paste(Vars[i], "MeanMSEn", sep=""),
      		paste(Vars[i], "VarRBn", sep="")
		)
		resultslist[[i]] %<>% 
			rename(
				!!cnames[1] := mean_RB_true_mean,
				!!cnames[2] := RB,
				!!cnames[3] := MSE,
				!!cnames[4] := var_RB,
				# rename sample size Vars
				!!cnames[5] := RB_n,
				!!cnames[6] := MSESampleSize,
				!!cnames[7] := var_RB_n
			) %>%
			dplyr::select(-c(
				.data$MeanRBMeanOfObsMean,
				.data$MSESumObs_True2,
				.data$varRBMeanOfVarEst,
				.data$varRBMeanOfVarEst_n,
				.data$varRBVarOfMeanEst,
				.data$varRBVarOfMeanEst_n
			))
	}
	return(resultslist)
}
calcMeanObsMeans <- function(dataframe, Vars, nsims, popgroupvar, samplinggroupvar) {
	temp <- dataframe %>% 
		group_by_at(c(
			popgroupvar, samplinggroupvar, "n_sims"
		))
	C <- vector("list", length(Vars))
	# for each of the popgroupvar and samplinggroupvar, calculate:
	#	mean of observed means
	#	sample size used for those calculations
	for (i in 1:length(Vars)) {
		C[[i]] <- list()
		# observed mean
		var = sym(paste(Vars[i],"MeanObs",sep=""))		
		C[[i]] <- temp %>%
			summarise(
				MeanObsMeans = mean(!!var, na.rm=T),
				sample_size = length(!is.na(!!var))
			)
		cnames <- c(
			paste(Vars[i], "_MeanObsMeans", sep=""),
			paste(Vars[i], "_MeanObsMeans_n", sep="")
		)
		C[[i]] %<>% 
			rename(
				!!cnames[1] := MeanObsMeans,
				!!cnames[2] := sample_size
			)
			
	}
	D <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(popgroupvar, samplinggroupvar, "n_sims")
		),
		C
	)
	dataframe %<>% merge(
		D, 
		by=c(popgroupvar, samplinggroupvar,"n_sims")
	)
	return(dataframe)
}

#' Calculate Simulation Data Sampling Bias
#' 
#' @param popdatasummary Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
#' @param simulation_data Simulation data on sampling of the multiple patch realizations.
#' @param popgroupvar Categorical variables with which to group the population data (e.g., artificial population number if there are more than 1)
#' @param samplinggroupvar Categorical variables with which to group the simulation data (e.g., sampling design used, number of primary samples).
#' @param ovar Vector of variables for which sampling bias should be estimated.
#' @param orvar Vector of variables for which secondary variables should be estimated. Can be identical to ovar or a subset.
#' @param rvar Variables for which to use ratio estimators

#' @description Calculate the sampling bias of different sampling designs from simulation data.

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
	popdatasummary, 
	simulation_data, 
	popgroupvar, 
	samplinggroupvar,
	ovar,
	orvar,
	rvar
)
{
	rvarnames <- paste(rvar, "_ratio", sep="")
	. <- n_sims <- A <- B <- C <- D <- E <- I <- G <- H <- NULL
	A <- merge(
		popdatasummary, 
		simulation_data, 
		by=popgroupvar
	)
	# ------------------------------------------------------------------------ #
	# OCCUPANCY Vars	
	# ------------------------------------------------------------------------ #
	B <- A 

	# add number of simulations to dataset
	B %<>% 
		group_by_at(c(popgroupvar, samplinggroupvar)) %>%
		mutate(n_sims = n())
	
	B %<>% calcMeanObsMeans(dataframe=B, Vars=ovar)
	
	# WHY IS IT ONLY CALC MEAN OF OBS MEANS FOR RVAR?
	
	
	
	
	
	
	
	# ------------------------------------------------------------------------ #
	# RATIO Vars	
	# ------------------------------------------------------------------------ #
	E <- A
	# E %<>% filter(StrictaMeanObs!=0)
	
	E %<>% calcMeanObsMeans(dataframe=B, Vars=c(rvarnames))
	
	E %<>% calcSqDiff(Vars=c(orvar, rvarnames))
	E %<>% calcDiffMeans(Vars=c(orvar, rvarnames))

	B %<>% calcSqDiff(Vars=ovar)
	B %<>% calcDiffMeans(Vars=ovar)
	
	# calculate bias and MSE
	# occupancy Vars
	H <- vector("list", length(ovar))
	X.grp <- B %>% 
		group_by_at(c(
			popgroupvar, 
			samplinggroupvar, 
			"n_sims"
		)) %>% 
		calculateBiasComponents(., resultslist=H, Vars=ovar)
	
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				popgroupvar, 
				samplinggroupvar, 
				"n_sims"
			)
		),
		X.grp
	) %>%
		setnames("n_sims", "ovar_n_sims")
	# ratio Vars
	H <- vector("list", length(rvar))
	
	
	
	

	
	
	X.grp <- E %>% 
		group_by_at(c(
			popgroupvar, 
			samplinggroupvar, 
			"n_sims"
		)) %>% 
		calculateBiasComponents(., resultslist=H, Vars=rvarnames)
	Z <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(popgroupvar, samplinggroupvar, "n_sims")
		),
		X.grp
	) %>%
		rename("rvar_n_sims" = "n_sims")
	Y %<>% merge(Z, 
		by=c(
			popgroupvar, 
			samplinggroupvar
		),
		all=T
	)
	
	return(Y)
}