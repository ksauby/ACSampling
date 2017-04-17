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
	population_data %<>% arrange_(.dots=population.grouping.variable)
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
	Z = population_data %>%
		group_by_(.dots=population.grouping.variable) %>%
		summarise(N = length(m)) %>%
		ungroup %>%
		as.data.frame
	Y1 %<>% merge(Y2, by=population.grouping.variable) %>%
		merge(Z, by=population.grouping.variable)	
	# spatial statistics and other characteristics of variables
	A <- list()
	for (i in 1:length(unique(eval(parse(
		text=paste(
			"population_data$", 
			population.grouping.variable, 
			sep=""
	)))))) 
	{
		temp <- population_data[which(
			eval(parse(
				text=paste(
					"population_data$", 
					population.grouping.variable, 
					sep=""
				))
			) == unique(eval(parse(
				text=paste(
					"population_data$", 
					population.grouping.variable, 
					sep=""
				)
			)))[i]
		), ]
		temp %<>% arrange(x,y)
		# spatial statistics
		coordinates(temp) = ~ x+y
		A[[i]] <- list()
		for (j in 1:length(summary.variables)) {
			tempvar <- eval(parse(text =
				paste("temp$", summary.variables[j], sep="")
			))
			Mean_tempvar 	<- Mean(tempvar)
			Var_tempvar 	<- PopVariance(tempvar)
			CV_tempvar 		<- popCV(tempvar)
			Total_tempvar 	<- Sum(tempvar)
			SSQ_R			<- calculateSSQR(
				patch_data = as.data.frame(temp),
				variable = summary.variables[j],
				population.grouping.variable
			)$SSQ_R
			# variogram information
			
			if (length(tempvar[which(tempvar > 0)]) > 0) {
				A1 <- autofitVariogram(
					eval(parse(text=summary.variables[j])) ~ 1,
					temp
				)$var_model
				# nugget: y-intercept
				semivar_nugget 	<- A1[1, ]$psill
				# psill, partial sill (?): asymptote
				partial_sill 	<- A1[2, ]$psill
				# range: lag at which the sill is reached
				semivar_range 	<- A1[2, ]$range
				# join counts and moran's i
				nb <- cell2nb(nrow = 30, ncol = 30)
				lwb <- nb2listw(nb, style = "S") # convert to weights
				# I think cells are indexed by row, then column
				JoinCountTest <- joincount.test(as.factor(
					eval(parse(text=paste(
						"temp$",
						summary.variables[j],
						sep=""
					)))),
					lwb
				)[[2]]$estimate[1]
				MoranI <- moran.test(
					eval(parse(text=paste(
						"temp$",
						summary.variables[j],
						sep=""
					))),
					lwb
				)$estimate[1]
			} else {
				semivar_nugget 	<- NA
				partial_sill 	<- NA 
				semivar_range 	<- NA 
				JoinCountTest 	<- NA 
				MoranI 			<- NA 
			}
			A[[i]][[j]] <- data.frame(
				Mean_tempvar,
				Var_tempvar,
				CV_tempvar,
				Total_tempvar,
				semivar_nugget,
				partial_sill,
				semivar_range,
				JoinCountTest,
				MoranI,
				SSQ_R,
				variable = summary.variables[j],
				row.names = NULL
			)
		}
		A[[i]] <- do.call(rbind.data.frame, A[[i]])
	
	
	
		
		A[[i]]$population <- unique(eval(parse(
			text=paste(
				"population_data$", 
				population.grouping.variable, 
				sep=""
			)
		)))[i]
	}
	B <- do.call(rbind.data.frame, A)
	B %<>% arrange(variable, population)
	if (!(is.null(ratio.variables))) {
		for (l in 1:length(ratio.variables)) {
			temp <- B %>% filter(variable ==ratio.variables[l])
			AuxVar <- B %>% filter(variable ==str_sub(ratio.variables[l],-7,-1))
			temp$Mean_tempvar <- temp$Total_tempvar/AuxVar$Total_tempvar
			B %<>% filter(variable !=ratio.variables[l]) %>%
				rbind.fill(temp)
		}
	}
	B %<>%
		setnames("Mean_tempvar", "Mean") %>%
		setnames("Var_tempvar", "Var") %>%
		setnames("CV_tempvar", "CV") %>%
		setnames("Total_tempvar", "Total")
	return(list(Y1, B))
}