#' Calculate Summary Statistics for Patch Population Data
#' 
#' @param population_data Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchPopulations} function).
#' @param summary.variables Vector of variables for which summary statistics should be calculated.
#' @param population.grouping.variable Categorical variable identifying the different populations.
#' @param ratio.variables Variables for which to use ratio estimators
#' @param weights Vector of spatial weight matrix styles. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.
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
	population.grouping.variable,
	weights="S"
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
			A[[i]][[j]] <- data.frame(variable = summary.variables[j])
			tempvar <- eval(parse(text =
				paste("temp$", summary.variables[j], sep="")
			))
			A[[i]][[j]]$Mean_tempvar 	<- Mean(tempvar)
			A[[i]][[j]]$Var_tempvar 	<- PopVariance(tempvar)
			A[[i]][[j]]$CV_tempvar 		<- popCV(tempvar)
			A[[i]][[j]]$Total_tempvar 	<- Sum(tempvar)
			A[[i]][[j]]$SSQ_R			<- calculateSSQR(
				patch_data = as.data.frame(temp),
				variable = summary.variables[j],
				population.grouping.variable
			)$SSQ_R
			if (length(tempvar[which(tempvar > 0)]) > 0) {
				# join counts and moran's i
				nb <- cell2nb(nrow = 30, ncol = 30)
				if ("W" %in% weights) {
					lwb <- nb2listw(nb, style = "W") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.W <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.W <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}
				if ("B" %in% weights) {
					lwb <- nb2listw(nb, style = "B") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.B <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.B <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("C" %in% weights) {
					lwb <- nb2listw(nb, style = "C") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.C <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.C <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("U" %in% weights) {
					lwb <- nb2listw(nb, style = "U") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.U <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.U <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("S" %in% weights) {
					lwb <- nb2listw(nb, style = "S") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.S <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.S <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("minmax" %in% weights) {
					lwb <- nb2listw(nb, style = "minmax") # convert to weights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.minmax <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.minmax <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summary.variables[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
			} else {
				if ("W" %in% weights) {
					A[[i]][[j]]$JoinCountTest.W <- NA
					A[[i]][[j]]$MoranI.W <- NA
				}
				if ("B" %in% weights) {
					A[[i]][[j]]$JoinCountTest.B <- NA
					A[[i]][[j]]$MoranI.B <- NA
				}	
				if ("C" %in% weights) {
					A[[i]][[j]]$JoinCountTest.C <- NA
					A[[i]][[j]]$MoranI.C <- NA
				}	
				if ("U" %in% weights) {
					A[[i]][[j]]$JoinCountTest.U <- NA
					A[[i]][[j]]$MoranI.U <- NA
				}	
				if ("S" %in% weights) {
					A[[i]][[j]]$JoinCountTest.S <- NA
					A[[i]][[j]]$MoranI.S <- NA
				}	
				if ("minmax" %in% weights) {
					A[[i]][[j]]$JoinCountTest.minmax <- NA
					A[[i]][[j]]$MoranI.minmax <- NA
				}	
			}
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