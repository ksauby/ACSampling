#' Calculate Summary Statistics for Patch Population Data
#' 
#' @param population_data Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchPopulations} function).
#' @param summary.variables Vector of variables for which summary statistics should be calculated.
#' @param population.grouping.variable Categorical variable identifying the different populations.
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
#' cactus.realizations <- createSpeciesPatchPopulations(x_start, x_end,
#' 	y_start, y_end, buffer, n.networks, n.realizations, SpeciesInfo, start.seed,
#' 	occupancy.variables)
#' patch_data_summary <- calculatePopulationSummaryStatistics(cactus.realizations, 
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

calculatePopulationSummaryStatistics <- function(
	population_data, 
	summary.variables, 
	ratio.variables=NULL, 
	population.grouping.variable
) {
	funs <- . <- m <- NetworkID <- NULL
	# for each population.grouping.variable combo, calculate mean, var, sum
	X = population_data %>%
		group_by_(.dots=population.grouping.variable) %>%
		dplyr::select_(.dots=lapply(
			c(
				summary.variables, 
				ratio.variables
			), 
			as.symbol
		)) %>% 
		summarise_each(
			funs(
				mean(., na.rm=T), 
				sum(., na.rm=T),
				PopVariance,
				popCV
			)
		) %>%
		ungroup
	if (length(c(summary.variables, ratio.variables)) == 1) {
		X %<>%
		setnames(
			., 
			"mean", 
			paste(
				c(summary.variables, ratio.variables), 
				"_", 
				"mean", 
				sep=""
			)
		) %>%
		setnames(
			., 
			"sum", 
			paste(
				c(summary.variables, ratio.variables), 
				"_", 
				"sum", 
				sep="" 
			)
		)
	} 
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
	# for each population.grouping.variable combo, calculate summary statistics for m and number of species patches
	# this calculates the m statistics for the unique Network sizes
	Y1 = population_data %>%
		group_by_(.dots=lapply(
			c("NetworkID", population.grouping.variable), 
			as.symbol
		)) %>%
		summarise(m = m[1]) %>%
		group_by_(.dots=population.grouping.variable) %>%
		summarise(
			m_min_unique_neigh = min(m),
			m_max_unique_neigh = max(m),
			m_mean_unique_neigh = mean(m),
			m_var_unique_neigh = var(m),
			n_Species_Patches = length(unique(NetworkID[which(m>1)]))
		) %>%
		ungroup %>%
		as.data.frame
	# this calculates the m statistics for all units
	Y2 = population_data %>%
		group_by_(.dots=population.grouping.variable) %>%
		summarise(
			m_min = min(m),
			m_max = max(m),
			m_mean = mean(m),
			m_var = var(m)
		) %>%
		ungroup %>%
		as.data.frame
	# spatial statistics	
	for (i in 1:length(unique(eval(parse(text=paste("population_data$", population.grouping.variable, sep="")))))) {
		temp <- patch_data %>% 
			filter(
				## NEED TO DO THIS FOR SAMPLE SPECIES REALIZATIONS AS WELL
				eval(parse(text=population.grouping.variable))) ==
				unique(eval(parse(
					text=paste(
						"population_data$", 
						population.grouping.variable, 
						sep=""
					)
				)))[i]
			)
		coordinates(temp) = ~ x+y
		for (j in 1:length(summary.variables)) {
			# variogram information
			A1 <- autofitVariogram(
				eval(parse(text=summary.variables[j])) ~ 1,
				temp
			)$var_model
			A2 <- var5_parms[1, ]$psill
			A3 <- var5_parms[2, ]$psill
			A4 <- var5_parms[2, ]$range
			# join counts and moran's i
			nb <- cell2nb(nrow = 30, ncol = 30)
			lwb <- nb2listw(nb, style = "S") # convert to weights
			A5 <- joincount.test(as.factor(
				temp$eval(parse(text=summary.variables[j]))),
				lwb
			)[[2]]$estimate[1]
			A6 <- moran.test(
				temp$eval(parse(text=summary.variables[j])), 
				lwb
			)$estimate[1]
		}		
	}
	Z
	
	
	
	A = population_data %>%
		group_by_(.dots=population.grouping.variable) %>%
		summarise(N = length(m)) %>%
		ungroup %>%
		as.data.frame
	X %<>% merge(Y1, by=population.grouping.variable) %>%
		merge(Y2, by=population.grouping.variable) %>%
		merge(A, by=population.grouping.variable)	
	return(X)
}