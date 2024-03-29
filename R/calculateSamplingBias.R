calcSqDiff <- function(dataframe, V) {
	# for each simulation and V, calculate:
	#	(observed mean - true mean)^2
	for (i in 1:length(V)) {
		cname = paste(V[i], "MeanObs_True2", sep="")
		dataframe %<>%
			mutate(
			     # (observed - true)^2
			     Obs_True2 = (
					eval(parse(text=paste(V[i], "MeanObs", sep=""))
					) - 
					eval(parse(text = paste(V[i], "TrueMean", sep=""))
				     )
				)^2
			) %>%
			rename(!!cname := Obs_True2)
	}
	return(dataframe)
}

calcDiffMeans <- function(dataframe, V) {
	# for each simulation and variable, calculate:
	#	observed mean - mean of observed means
	for (i in 1:length(V)) {
		cname = paste(
		     V[i], 
		     "_Obs_MeanObsMeans", 
		     sep=""
		)
		dataframe %<>%
			mutate(
			     # (observed - true)
			     Obs_MeanObsMeans = 
					eval(parse(text = paste(V[i], "MeanObs", sep=""))) - 
					eval(parse(text = paste(V[i], "_MeanObsMeans",
					     sep = "")))
			) %>%
			rename(!!cname := Obs_MeanObsMeans)
	}
	return(dataframe)
}

#' @importFrom lazyeval interp
#' @importFrom rlang .data :=
#' @importFrom dplyr rename

calcBiasComponents	<- function(dataframe, resultslist, V) {
	for (i in 1:length(V)) {
		resultslist[[i]] <- list()
		VAR <- sym(paste0(V[i], "TrueMean"))
		OBSMEAN <- sym(paste0(V[i], "MeanObs"))
		OBSMEAN_TRUE2 <- sym(paste0(V[i], "MeanObs_True2"))
		OBSVAR <- sym(paste0(V[i], "VarObs"))
		resultslist[[i]] <- dataframe %>%
			summarise(
				# save true mean
				TrueMean = var(!!VAR)[1],
				MeanOfObsMean = mean(!!OBSMEAN, na.rm=TRUE),
				RB_n = length(!is.na(!!OBSMEAN)),
				MSESumObs_True2 = sum(!!OBSMEAN_TRUE2, na.rm = TRUE), 
				MSESampleSize = length(!is.na(!!OBSMEAN_TRUE2)),
				# for var_RB:
				#  calculate mean of simulated HT variance estimates
				MeanVarEst = mean(!!OBSVAR, na.rm = TRUE), 
				#  sample size used to calc mean of simulated HT variance estimates
				MeanVarEst_n = length(!is.na(!!OBSVAR)),
				#  calculate variance of MeanObs
				VarMeanEst = var(!!OBSMEAN, na.rm = TRUE),
				#  calculate sample size used to calculate variance of MeanObs
				VarMeanEst_n = length(!is.na(!!OBSMEAN))
			)
		resultslist[[i]] %<>% 
			mutate(
				RB = 100 * (.data$MeanOfObsMean - .data$TrueMean) / .data$TrueMean,
				MSE = .data$MSESumObs_True2/.data$MSESampleSize,
				var_RB = (.data$MeanVarEst - .data$VarMeanEst)/.data$VarMeanEst,
				var_RB_n = min(.data$MeanVarEst_n, .data$VarMeanEst_n)
			)
		cnames <- c(
      		paste(V[i], "TrueMean", sep=""),
      		paste(V[i], "MeanRB", sep=""),
      		paste(V[i], "MeanMSE", sep=""),
      		paste(V[i], "VarRB", sep=""),
			# rename sample size V
			paste(V[i], "MeanRBn", sep=""),
      		paste(V[i], "MeanMSEn", sep=""),
      		paste(V[i], "VarRBn", sep="")
		)
		resultslist[[i]] %<>% 
			rename(
				!!cnames[1] := TrueMean,
				!!cnames[2] := RB,
				!!cnames[3] := MSE,
				!!cnames[4] := var_RB,
				# rename sample size V
				!!cnames[5] := RB_n,
				!!cnames[6] := MSESampleSize,
				!!cnames[7] := var_RB_n
			) %>%
			dplyr::select(-c(
				.data$MeanOfObsMean,
				.data$MSESumObs_True2,
				.data$MeanVarEst,
				.data$MeanVarEst_n,
				.data$VarMeanEst,
				.data$VarMeanEst_n
			))
	}
	return(resultslist)
}

calcMeanObsMeans <- function(dataframe, V, nsims, popvar, samplinggroupvar) {
   POPVAR <- sym(popvar)
   SAMPLINGGROUPVAR <- sym(samplinggroupvar)
   NSIMS <- sym("n_sims")
	temp <- dataframe %>% 
		group_by(POPVAR, SAMPLINGGROUPVAR, NSIMS)
	C <- vector("list", length(V))
	# for each of the popvar and samplinggroupvar, calculate:
	#	mean of observed means
	#	sample size used for those calculations
	for (i in 1:length(V)) {
		C[[i]] <- list()
		# observed mean
		var = sym(paste(V[i],"MeanObs",sep=""))		
		C[[i]] <- temp %>%
			summarise(
				MeanObsMeans = mean(!!var, na.rm=T),
				sample_size = length(!is.na(!!var))
			)
		cnames <- c(
			paste(V[i], "_MeanObsMeans", sep=""),
			paste(V[i], "_MeanObsMeans_n", sep="")
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
			by=c(popvar, samplinggroupvar, "n_sims")
		),
		C
	)
	dataframe %<>% merge(
		D, 
		by=c(popvar, samplinggroupvar,"n_sims")
	)
	return(dataframe)
}

#' Calculate Simulation Data Sampling Bias
#' 
#' @param popdatasummary Summary statistics on the species patch realizations of patches (created by \code{calculateRealizationSummaryStatistics} function).
#' @param simdata Simulation data on sampling of the multiple patch realizations.
#' @template popvar
#' @template samplinggroupvar
#' @template ovar
#' @param orvar Vector of variables for which secondary variables should be estimated. Can be identical to \code{ovar} or a subset.
#' @template rvar

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
#' cactus.realizations = createRealizations(x_start, x_end,
#' 	y_start, y_end, buffer=5, n.networks, n.realizations, SpeciesInfo, 
#' 	start.seed=1, Species.Fields, yvar="Cactus")
#' 
#' # Sample from the realizations
#' simulations=1
#' n1_vec=c(5,10,20,40)
#' population <- createPop(x_start = 1, x_end = 30, y_start = 1, 
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
#' 	# "Height_Stricta"
#' )		
#' patch_data = cactus.realizations
#' # simdata <- sampleSpeciesPatchRealizations(patch_data, simulations, 
#' #	n1_vec, population, abundance.variables, occupancy.variables)
#' 
#' # summary.variables = occupancy.variables
#' # grouping.variables = c("n.networks", "realization")
#' # dataset = cactus.realizations
#' # patch_data_summary <- calculateRealizationSummaryStatistics(dataset, 
#' # 	summary.variables, grouping.variables)

#'sims=200
#'n1=c(5,10,20,40)
#'population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
#'avar = NULL
#'ovar = c(
#'	"Stricta",
#'	"Pusilla",
#'	"Cactus",
#'	"CACA_on_Pusilla",
#'	"CACA_on_Stricta",
#'	"MEPR_on_Pusilla",
#'	"MEPR_on_Stricta",
#'	"Old_Moth_Evidence_Pusilla",
#'	"Old_Moth_Evidence_Stricta"
#'	# "Percent_Cover_Pusilla", # how do I do these? they are occupancy nor abundance
#'	# "Percent_Cover_Stricta",
#'	# "Height_Pusilla",
#'	# "Height_Stricta",
#')		
#'popdata = cactus.realizations
#' #simulation_data <- sampleRealizations(popdata, sims, 
#'#n1, population, avar, ovar)

#'#CactusRealizationSummary <- calcPopSummaryStats(
#'#	popdata = CactusRealizations, 
#'#	summaryvar = c("Stricta", "Pusilla", "Cactus",
#'#		"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#'#		"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
#'#	popvar = "population", 
#'#	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", 
#'#		"Percent_Cover_Stricta", "Height_Stricta", 
#'#		"Old_Moth_Evidence_Stricta"),
#'#	nrow=30,
#'#	ncol=30
#'#)
#'#patch_data_summary_wide <- createWidePopSummaryStats(
#'#	popsummarystats = CactusRealizationSummary,
#'#	ovar = "Stricta",
#'#	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#'#		"Height_Stricta", "Old_Moth_Evidence_Stricta")
#'#)



#'#simdata_summary_table_re = calcSamplingBias(
#'#	popdatasummary	= patch_data_summary_wide, 
#'#	simdata		= simdata_all_re, 
#'#	sampgroupvar	= sampgroupvar, 
#'#	popvar = popvar,
#'#	ovar			= ovar, 
#'#	rvar				= rvar
#'#)



#' # avar = NULL
#' @export

calcSamplingBias <- function(
	popdatasummary, 
	simdata, 
	popvar, 
	samplinggroupvar,
	ovar,
	orvar,
	rvar
)
{
   
   POPVAR <- sym(popvar)
   SAMPLINGGROUPVAR <- sym(samplinggroupvar)
   NSIMS <- sym("n_sims")
   
   
	rvarnames <- paste(rvar, "_ratio", sep="")
	. <- n_sims <- A <- B <- C <- D <- E <- I <- G <- H <- NULL
	A <- merge(
		popdatasummary, 
		simdata, 
		by=popvar
	)
	# ------------------------------------------------------------------------ #
	# OCCUPANCY Vars	
	# ------------------------------------------------------------------------ #
	B <- A 

	# add number of simulations to dataset
	B %<>% 
	   group_by(POPVAR, SAMPLINGGROUPVAR) %>%
		mutate(n_sims = n())
	
	B %<>% calcMeanObsMeans(dataframe=B, V=ovar)
	
	# WHY IS IT ONLY CALC MEAN OF OBS MEANS FOR RVAR?
	
	
	
	
	
	
	
	# ------------------------------------------------------------------------ #
	# RATIO Vars	
	# ------------------------------------------------------------------------ #
	E <- A
	# E %<>% filter(StrictaMeanObs!=0)
	
	E %<>% calcMeanObsMeans(dataframe=B, V=c(rvarnames))
	
	E %<>% calcSqDiff(V=c(orvar, rvarnames))
	E %<>% calcDiffMeans(V=c(orvar, rvarnames))

	B %<>% calcSqDiff(V=ovar)
	B %<>% calcDiffMeans(V=ovar)
	
	# calculate bias and MSE
	# occupancy Vars
	H <- vector("list", length(ovar))
	X.grp <- B %>% 
	   group_by(POPVAR, SAMPLINGGROUPVAR, NSIMS) %>% 
		calcBiasComponents(., resultslist=H, V=ovar)
	
	Y <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(
				popvar, 
				samplinggroupvar, 
				"n_sims"
			)
		),
		X.grp
	) %>%
		rename("ovar_n_sims" = "n_sims")
	# ratio Vars
	H <- vector("list", length(rvar))
	
	
	
	

	
	
	X.grp <- E %>% 
		group_by_at(c(
			popvar, 
			samplinggroupvar, 
			"n_sims"
		)) %>% 
		calcBiasComponents(., resultslist=H, V=rvarnames)
	Z <- Reduce(
		function(x, y) merge(
			x, y,
			by=c(popvar, samplinggroupvar, "n_sims")
		),
		X.grp
	) %>%
		rename("rvar_n_sims" = "n_sims")
	Y %<>% merge(Z, 
		by=c(
			popvar, 
			samplinggroupvar
		),
		all=T
	)
	
	return(Y)
}