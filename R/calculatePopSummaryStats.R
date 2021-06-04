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
#' @param popdata Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchPopulations} function).
#' @param summaryvar Vector of variables for which summary statistics should be calculated.
#' @param popgroupvar String identifying the categorical variable identifying the different populations.
#' @param rvar Vector of variables for which ratio estimators should be used.
#' @param spatweights Vector of spatial weight matrix styles. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.
#' @param nrow the number of rows in the grid that creates the population
#' @param ncol the number of columns in the grid that creates the population
#' @description Calculates summary statistics for patch population data.

#' @return Dataframe including summary statistics for each column identified in \code{summaryvar} and for each category identified in \code{popgroupvar}.

#' @export
#' @importFrom dplyr ungroup sym group_by_at
#' @importFrom stringr str_sub
#' @importFrom stats var
#' @importFrom sp coordinates
#' @importFrom spdep cell2nb nb2listw joincount.test moran.test 

#' @examples
#' library(magrittr)
#' library(dplyr)
#' ovar = c(
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
#' summaryvar = ovar
#' # WHAT WAS I THINK HERE? for grouping variables?
#' popgroupvar = "n.networks" # c("n.networks", "realization")
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
#' rvar = c("CACA_on_Stricta", "CACA_on_Pusilla")
#' cactus.realizations <- createSpeciesPatchRealizations(x_start, x_end,
#' 	y_start, y_end, buffer, n.networks, n.realizations, SpeciesInfo, start.seed,
#' 	ovar)
#' patch_data_summary <- calcPopSummaryStats(cactus.realizations, 
#' 	summaryvar=ovar, popgroupvar=popgroupvar, nrow=30, ncol=30)
	
calcPopSummaryStats <- function(
	popdata, 
	summaryvar, 
	rvar=NULL, 
	popgroupvar,
	spatweights="S",
	nrow,
	ncol
) {
	POPVAR <- sym(popgroupvar)
	popdata %<>% arrange(!!POPVAR)
	# for each popgroupvar combo, calculate summary statistics for m and number of species patches
	# this calculates the m statistics for the unique Network sizes
	Y1 <- popdata %>%
		group_by_at(c("NetworkID", popgroupvar)) %>%
		summarise(m = .data$m[1]) %>%
		group_by_at(popgroupvar) %>%
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
	group_by_at(popgroupvar) %>%
		summarise(
			m_min = min(.data$m),
			m_max = max(.data$m),
			m_mean = mean(.data$m),
			m_var = var(.data$m)
		) %>%
		ungroup %>%
		as.data.frame
	Z = popdata %>%
		group_by_at(popgroupvar) %>%
		summarise(N = length(.data$m)) %>%
		ungroup %>%
		as.data.frame
	Y1 %<>% 
		merge(Y2, by=popgroupvar) %>%
		merge(Z, by=popgroupvar)	
	# spatial statistics and other characteristics of variables
	A <- list()
	popvar <- paste(
		"popdata$", 
		popgroupvar, 
		sep=""
	)
	for (i in 1:length(unique(eval(parse(text=popvar))))) {
		temp <- popdata %>%
			filter(!!POPVAR == unique(!!POPVAR)[i])
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
			A[[i]][[j]]$Mean_tempvar <- Mean(tempvar)
			A[[i]][[j]]$Var_tempvar <- PopVariance(tempvar)
			A[[i]][[j]]$CV_tempvar <- popCV(tempvar)
			A[[i]][[j]]$Total_tempvar <- Sum(tempvar)
			A[[i]][[j]]$SSQ_R <- calcSSQR(
				popdata = as.data.frame(temp),
				variable = summaryvar[j],
				popgroupvar
			)$SSQ_R
			# for join counts and moran's i, change NAs in rvar's to zeros
			temp2 <- temp
			temp2@data[rvar][is.na(temp2@data[rvar])] <- 0
			if (length(tempvar[which(tempvar > 0)]) > 0) {
				# join counts and moran's i
				nb <- cell2nb(nrow = nrow, ncol = ncol)
				if ("W" %in% spatweights) {
					lwb <- nb2listw(nb, style = "W") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.W <- joincount.test(
					     as.factor(
                                   eval(
                                        parse(
                                             text=paste(
                                                  "temp2$",
                                                  summaryvar[j],
                                                  sep = ""
                                             )
                                        )
                                   )
                              ),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.W <- moran.test(
						eval(
                                   parse(
                                        text=paste(
                                             "temp2$",
                                             summaryvar[j],
                                             sep=""
                                        )
                                   )
						),
						lwb
					)$estimate[1]
				}
				if ("B" %in% spatweights) {
					lwb <- nb2listw(nb, style = "B") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.B <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.B <- moran.test(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("C" %in% spatweights) {
					lwb <- nb2listw(nb, style = "C") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.C <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.C <- moran.test(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("U" %in% spatweights) {
					lwb <- nb2listw(nb, style = "U") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.U <- joincount.test(as.factor(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.U <- moran.test(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("S" %in% spatweights) {
					lwb <- nb2listw(nb, style = "S") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.S <- joincount.test(as.factor(
						# NEED TO FIGURE OUT HOW TO GET RID OF NAs
						# OR CAN YOU JUST NOT DO JOINT COUNT TEST FOR RVAR
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						)))),
						lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.S <- moran.test(
						eval(parse(text=paste(
							"temp2$",
							summaryvar[j],
							sep=""
						))),
						lwb
					)$estimate[1]
				}	
				if ("minmax" %in% spatweights) {
					lwb <- nb2listw(nb, style = "minmax") # convert to spatweights
					# I think cells are indexed by row, then column
					A[[i]][[j]]$JoinCountTest.minmax <- joincount.test(
                              as.factor(
                                   eval(
                                        parse(
                                             text = paste(
                                                  "temp2$",
                                                  summaryvar[j],
                                                  sep = ""
                                             )
                                        )
                                   )
                              ),
					     lwb
					)[[2]]$estimate[1]
					A[[i]][[j]]$MoranI.minmax <- moran.test(
                              eval(
                                   parse(
                                        text=paste(
                                             "temp2$",
                                             summaryvar[j],
                                             sep=""
                                        )
                                   )
                              ),
						lwb
					)$estimate[1]
				}	
			} else {
				if ("W" %in% spatweights) {
					A[[i]][[j]]$JoinCountTest.W <- NA
					A[[i]][[j]]$MoranI.W <- NA
				}
				if ("B" %in% spatweights) {
					A[[i]][[j]]$JoinCountTest.B <- NA
					A[[i]][[j]]$MoranI.B <- NA
				}	
				if ("C" %in% spatweights) {
					A[[i]][[j]]$JoinCountTest.C <- NA
					A[[i]][[j]]$MoranI.C <- NA
				}	
				if ("U" %in% spatweights) {
					A[[i]][[j]]$JoinCountTest.U <- NA
					A[[i]][[j]]$MoranI.U <- NA
				}	
				if ("S" %in% spatweights) {
					A[[i]][[j]]$JoinCountTest.S <- NA
					A[[i]][[j]]$MoranI.S <- NA
				}	
				if ("minmax" %in% spatweights) {
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
	B %<>% arrange(variable, population) %>%
		dplyr::rename(
			Mean = Mean_tempvar,
			Var = Var_tempvar,
			CV = CV_tempvar,
			Total = Total_tempvar
		)
	return(list(Y1, B))
}