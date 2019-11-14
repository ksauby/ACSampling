#' Return the sum of a vector, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.

Sum <- function(x) {sum(x, na.rm=TRUE)}

#' Return the mean of a vector with up to two decimal places, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.

Mean <- function(x) base::mean(x, na.rm=TRUE)


#' Return the population variance of a vector, after removing NAs, and round.
#'
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.

PopVariance <- function(x) {
	temp <- sum((x-mean(x))^2)/length(x)
}

#' Population Coefficient of Variation
#' 
#' @param x Vectors of data.

popCV <- function(x) {sqrt(PopVariance(x))/Mean(x)}

#' Calculate Summary Statistics for Patch Population Data
#' 
#' @param population_data Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchPopulations} function).
#' @param summaryvar Vector of variables for which summary statistics should be calculated.
#' @param popgroupvar Categorical variable identifying the different populations.
#' @param rvar Vector of variables for which ratio estimators should be used.
#' @param weights Vector of spatial weight matrix styles. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.

#' @description Calculates summary statistics for patch population data.

#' @return Dataframe including summary statistics for each column identified in \code{summaryvar} and for each category identified in \code{grouping.variables}.

#' @export
#' @importFrom dplyr group_by_ ungroup arrange_
#' @importFrom stringr str_sub
#' @importFrom stats var
#' @importFrom sp coordinates
#' @importFrom spdep cell2nb nb2listw joincount.test moran.test 

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
#' summaryvar = occupancy.variables
#' # WHAT WAS I THINK HERE? for grouping variables?
#' grouping.variables = "n.networks" # c("n.networks", "realization")
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
#' patch_data_summary <- calculatePopulationSummaryStatistics(cactus.realizations, 
#' 	summaryvar=occupancy.variables, popgroupvar=grouping.variables)
	
calculatePopulationSummaryStatistics <- function(
	popdata, 
	summaryvar, 
	rvar=NULL, 
	popgroupvar,
	weights="S"
) {
	popdata %<>% arrange_(.dots=popgroupvar)
	# for each popgroupvar combo, calculate summary statistics for m and number of species patches
	# this calculates the m statistics for the unique Network sizes
	Y1 = popdata %>%
		group_by_(.dots=lapply(
			c("NetworkID", popgroupvar), 
			as.symbol
		)) %>%
		summarise(m = .data$m[1]) %>%
		group_by_(.dots=popgroupvar) %>%
		summarise(
			m_min_unique_neigh = min(.data$m),
			m_max_unique_neigh = max(.data$m),
			m_mean_unique_neigh = mean(.data$m),
			m_var_unique_neigh = var(.data$m),
			n_Species_Patches = 
				length(unique(.data$NetworkID[which(.data$m>1)]))
		) %>%
		ungroup %>%
		as.data.frame
	# this calculates the m statistics for all units
	Y2 = popdata %>%
		group_by_(.dots=popgroupvar) %>%
		summarise(
			m_min = min(.data$m),
			m_max = max(.data$m),
			m_mean = mean(.data$m),
			m_var = var(.data$m)
		) %>%
		ungroup %>%
		as.data.frame
	Z = popdata %>%
		group_by_(.dots=popgroupvar) %>%
		summarise(N = length(.data$m)) %>%
		ungroup %>%
		as.data.frame
	Y1 %<>% merge(Y2, by=popgroupvar) %>%
		merge(Z, by=popgroupvar)	
	# spatial statistics and other characteristics of variables
	A <- list()
	population_variable <- paste(
		"popdata$", 
		popgroupvar, 
		sep=""
	)
	for (i in 1:length(unique(eval(parse(text=population_variable))))) {
		temp <- popdata[which(
			eval(parse(text=population_variable)) == 
			unique(eval(parse(text=population_variable)))[i]
		), ]
		temp %<>% arrange(.data$x,.data$y)
		# spatial statistics
		coordinates(temp) = ~ x + y
		A[[i]] <- list()
		for (j in 1:length(summaryvar)) {
			A[[i]][[j]] <- data.frame(variable = summaryvar[j])
			if (summaryvar[j] %in% rvar) {
				temp_ratio <- temp %>% as.data.frame
				temp_ratio %<>% 
					.[.[colnames(.)==str_sub(summaryvar[j],-7,-1)]==1, ]	
				tempvar <- eval(parse(text =
						paste("temp_ratio$", summaryvar[j], sep="")
					))
				coordinates(temp_ratio) = ~ x + y
			} else {
				tempvar <- eval(parse(text =
					paste("temp$", summaryvar[j], sep="")
				))	
			}
			A[[i]][[j]]$Mean_tempvar 	<- Mean(tempvar)
			A[[i]][[j]]$Var_tempvar 	<- PopVariance(tempvar)
			A[[i]][[j]]$CV_tempvar 		<- popCV(tempvar)
			A[[i]][[j]]$Total_tempvar 	<- Sum(tempvar)
			A[[i]][[j]]$SSQ_R			<- calculateSSQR(
				patch_data = as.data.frame(temp),
				variable = summaryvar[j],
				popgroupvar
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.W <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.B <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.C <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.U <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.S <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.minmax <- moran.test(
						eval(parse(text=paste(
							"temp$",
							summaryvar[j],
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
				"popdata$", 
				popgroupvar, 
				sep=""
			)
		)))[i]
	}
	B <- do.call(rbind.data.frame, A)
	B %<>% arrange(.data$variable, .data$population) %>%
		setnames("Mean_tempvar", "Mean") %>%
		setnames("Var_tempvar", "Var") %>%
		setnames("CV_tempvar", "CV") %>%
		setnames("Total_tempvar", "Total")
	return(list(Y1, B))
}