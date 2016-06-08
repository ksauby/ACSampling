#' Calculate Summary Statistics for Patch Realization Data
#' 
#' @param dataset Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchRealizations} function).
#' @param summary.variables Vector of variables for which summary statistics should be calculated.
#' @param grouping.variables Categorical variables identifying the patch realization from which the simulation data was generated (e.g., \code{n.networks} and \code{realization}).
#' @param ratio.variables Variables for which to use ratio estimators
#' @return Dataframe including summary statistics for each column identified in \code{summary.variables} and for each category identified in \code{grouping.variables}.
#' @examples
#' library(magrittr)
#' library(dplyr)
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
#' summary.variables = occupancy.variables
#' grouping.variables = c("n.networks", "realization")
#' # create realizations
#' x_start = 1
#' x_end = 30
#' y_start = 1
#' y_end = 30
#' n.networks = c(5, 15, 10, 20, 30, 40)
#' n.realizations = 1
#' SpeciesInfo = PlotSurveys_season1
#' start.seed=1
#' buffer=5
#' cactus.realizations <- createSpeciesPatchRealizations(x_start, x_end,
#' 	y_start, y_end, buffer, n.networks, n.realizations, SpeciesInfo, start.seed,
#' 	occupancy.variables)
#' patch_data_summary <- calculateRealizationSummaryStatistics(cactus.realizations, 
#' 	summary.variables=occupancy.variables, grouping.variables=grouping.variables)
#' patch_data_summary %<>% 
#' 	round(3) %>% 
#' 	arrange(n.networks) %>% 
#' 	dplyr::select(
#' 		starts_with("Pusilla"), 
#' 		starts_with("Stricta"), 
#' 		starts_with("Cactus"), 
#' 		starts_with("CACA_on_Pusilla"),
#' 		starts_with("CACA_on_Stricta"),
#' 		starts_with("MEPR_on_Pusilla"),
#' 		starts_with("MEPR_on_Stricta"),
#' 		starts_with("Old_Moth_Evidence_Pusilla"),
#' 		starts_with("Old_Moth_Evidence_Stricta"),
#' 		everything()
#' 	)
#' write.csv(patch_data_summary, file=paste("patch_data_summary", 
#' format(Sys.time(), "%Y-%m-%d_%H-%M"), ".csv", sep=""))
#' @export
#' @importFrom dplyr group_by_
#' @importFrom dplyr ungroup
#' @importFrom stringr str_sub

calculateRealizationSummaryStatistics <- function(dataset, summary.variables, ratio.variables=NULL, grouping.variables) {
	funs <- . <- m <- NetworkID <- NULL
	# for each grouping.variables combo, calculate mean, var, sum
	X = dataset %>%
		group_by_(.dots=lapply(grouping.variables, as.symbol)) %>%
		dplyr::select_(.dots=lapply(c(summary.variables, ratio.variables), as.symbol)) %>% 
		summarise_each(
			funs(
				mean(., na.rm=T), 
				sum(., na.rm=T)
			)
		) %>%
		ungroup
	if (!(is.null(ratio.variables))) {
		for (l in 1:length(ratio.variables)) {
			y <- eval(parse(text=paste(
				"X$", 
				ratio.variables[l], 
				"_sum", 
				sep=""
			)))
			z <- eval(parse(text = paste(
				"X$", 
				str_sub(ratio.variables[l],-7,-1), 
				"_sum", 
				sep=""
			)))
			X[, dim(X)[2] + 1] <- ifelse(z!=0, y/z, 0)
			names(X)[dim(X)[2]] <- paste(
				ratio.variables[l], 
				"_ratio_mean", 
				sep=""
			)
		}
	}
	# for each grouping.variables combo, calculate summary statistics for m and number of species patches
	Y = dataset %>%
		group_by_(.dots=lapply(c("NetworkID", grouping.variables), as.symbol)) %>%
		summarise(m = m[1]) %>%
		group_by_(.dots=lapply(grouping.variables, as.symbol)) %>%
		summarise(
			m_min = min(m),
			m_max = max(m),
			m_mean = mean(m),
			m_var = var(m),
			n_Species_Patches = length(unique(NetworkID[which(m>1)]))
		) %>%
		ungroup %>%
		as.data.frame
	Z = dataset %>%
		group_by_(.dots=lapply(grouping.variables, as.symbol)) %>%
		summarise(N = length(m)) %>%
		ungroup %>%
		as.data.frame
	X %<>% merge(Y, by=grouping.variables) %>%
		merge(Z, by=grouping.variables)	
	return(X)
}