#' cactus patch realizations simulations

#' @param y_variable variable upon which adaptive cluster sampling criterion is based
#' @param patchdat patch realizations
#' @param simulations Number of simulations per population.
#' @param nsamples Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param avar Vector of variables for which abundance should be estimated.
#' @param ovar Vector of variables for which occupancy should be estimated.
#' @param rvar Vector of variables for which ratio estimators should be used.
#' @param SamplingDesign Whether restricted or unrestricted adaptive cluster sampling should be performed; defaults to \code{FALSE}.
#' @param y_HT_formula Default is "Thompson".
#' @param m_threshold Default is NULL.
#' @param f_max Default is 2.
#' @param SampleEstimators If "TRUE", calculate the sample mean and sample variance for each simulation. Default is FALSE.
#' @param SpatialStatistics If "TRUE", for each simulation calculate Moran's I, and the nugget, sill, and range of the semivariogram. Default is TRUE
#' @param weights If SpatialStatistics is "TRUE", this is a vector giving spatial weight matrix styles to use to calculate the Join Count and Moran's I statistics. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.
#' @param mCharacteristics If "TRUE", for each simulation calculate summary statistics (median, mean, min, and max) for the sample's m values. Also, for each simulation and for the set of unique m values, calculate the same summary statistics.
#' @param patch_variable Default is "n.networks"
#' @param realization_variable Default is "realization"

#' @description This function simulates sampling of multiple realizations of patches of the species of interest within the grid of locations created with \code{createPopulation}.

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @importFrom foreach foreach 
#' @importFrom foreach %dopar% 
#' @importFrom dplyr select_
 
#' @export

#' @examples
#' simulations=200
#' nsamples=c(5,10,20,40)
#' population <- createPopulation(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
#' avar = NULL
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
#' # patchdat = cactus.realizations
#' # simulation_data <- sampleSpeciesPatchRealizations(patchdat, simulations, 
#' # 	nsamples, population, avar, ovar)
#' # simulations=200
#' # #nsamples=c(75,150,225,300,350)
#' # simulation_data_SRSWOR <- sampleSpeciesPatchRealizations(patchdat, 
#' # 	simulations, nsamples, population, avar, ovar)
#' # # save data
#' # write.csv(simulation_data_SRSWOR, file=paste("simulation_data_SRSWOR", 
#' # format(Sys.time(), "%Y-%m-%d_%H-%M"), ".csv", sep=""))
#' #  write.csv(simulation_data, file=paste("simulation_data", format(Sys.time(), 
#' # "%Y-%m-%d_%H-%M"), ".csv", sep=""))

sampleSpeciesPatchRealizations <- function(
	patchdat, 
	simulations, 
	nsamples, 
	avar, 
	ovar, 
	rvar,
	#ACS=TRUE, 
	SamplingDesign="ACS",
	y_variable,
	y_HT_formula = "y_HT",
	var_y_HT_formula = "var_y_HT",
	m_threshold = NULL,
	f_max = 2,
	SampleEstimators = FALSE,
	SpatialStatistics = TRUE,
	mCharacteristics = TRUE,
	patch_variable = "n.networks",
	realization_variable = "realization",
	weights="S"
) 
{
	n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
	TIME 					<- Sys.time()
	patchdat 				%<>% arrange_(.dots=c(
								patch_variable,
								realization_variable
							))
	var 					<- c(ovar, avar, rvar)
	n.patches 				<- length(unique(eval(parse(text=paste(
								"patchdat$",
								patch_variable,
								sep=""
							)))))
	nsample.length 			<- length(nsamples)
	A 						<- vector("list", n.patches)
	# concatenate ovar and avar variables since the same code is used to calculate the Horvitz-Thompson estimators for occupancy and abundance
	oavar 					<- c(ovar, avar)
	# make empty data frames; these will be cbind'd together after Horvitz-Thompson estimators are calculated
	occ_abund_var 			<- data.frame(row.names = 1:length(c(ovar, avar))) 
	occ_abund_mean 			<- data.frame(row.names = 1:length(c(ovar, avar)))
	Ratio 					<- data.frame(row.names = 1:length(rvar)) 
	# the names to assign the estimates
	occ_abund_mean_names 	<- paste(ovar, avar, "_mean_observed", sep="")
	occ_abund_var_names 	<- paste(ovar, avar, "_var_observed", sep="")
	ratio_mean_names 		<- paste(rvar, "_ratio_mean_observed", sep="")
	ratio_var_names 		<- paste(rvar, "_ratio_var_observed", sep="")
	# i=1;j=1;k=1
	Z = foreach (
		i = 1:n.patches, # for each species density
		.inorder = FALSE, 
		.packages = c("magrittr", "foreach", "plyr", "dplyr", "data.table",
		 	"ACSampling", "intergraph", "network", "igraph", "stringr", "spdep"), 
		.combine = "rbind.fill",
		#.errorhandling = "pass",
		.verbose = TRUE
		) %:%
	 	foreach (
			j = 1:nsample.length, # for each sampling effort
			.combine = "rbind.fill",
			.inorder = FALSE
		) %dopar% {
			cat(paste(i,j, sep="_"))
			P 			<- patchdat %>% 
							filter(n.networks==unique(
								eval(parse(text=paste(
									"patchdat$",
									patch_variable,
									sep=""
								)))
							)[i])
			N 			<- dim(P)[1]
			n1 			<- nsamples[j]
			A[[i]][[j]] <- list()
			r 			<- (i - 1) * j + j
			seeds 		<- runif(simulations)
		    for (k in 1:simulations) {
				temp_seed <- seeds[k]*100000
				if (SamplingDesign=="ACS")
				{
					alldata <- createACS(
						population=P, 
						seed=temp_seed, 
						n1=n1, 
						y_variable=y_variable
					) %>% 
						as.data.table
				} else if (SamplingDesign=="RACS")
				{
					alldata <- createRACS_flex(
						population=P, 
						seed=temp_seed, 
						n1=n1, 
						y_variable=y_variable,
						f_max=f_max
					) %>% 
						as.data.table
				} else
				{
					alldata <- createSRS(
						population=P, 
						seed=temp_seed, 
						n1=n1
					) %>% 
						as.data.table
				}
				alldata_all <- alldata
				if (SampleEstimators == TRUE) {
					################ SRSWOR Sampling #####################
					if (SamplingDesign!="ACS" & SamplingDesign!="RACS") {
						# datasets to apply simple mean/variance and simple ratio estimator
						dats <- "alldata"
					}
					if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
						################ SRSWOR Data, alldata ################
						SRSWOR_data <- alldata %>% 
							filter(Sampling=="SRSWOR") %>% 
							as.data.table
						alldata %<>% filter(Sampling!="Edge") %>% 
							as.data.table
						# datasets to apply simple mean/variance and simple ratio estimatr
						dats <- c("SRSWOR_data", "alldata")
					}
					# datasets to apply simple mean/variance and simple ratio estimator
					# sample mean and variance applied to alldata, SRSWOR_data
					SampleMeanVar <- list()
					for (n in 1:length(dats)) {
						dat <- eval(parse(text=dats[[n]]))[, oavar, with=FALSE] %>% 
							summarise_each(funs(
								mean(., na.rm=T), 
								var(., na.rm=T)
							))
						setnames(
							dat,
							names(dat), 
							paste(names(dat), "observed", sep="_")
						)
						dat$Plots <- dats[n]
						SampleMeanVar[[n]] <- dat
					}
					SampleMeanVar %<>% rbind.fill
					# simple ratio estimators applied to alldata, SRSWOR_data
					if (!(is.null(rvar))) {
						SampleRatio <- list()
						for (n in 1:length(dats)) {
							SampleRatio[[n]] <- data.frame(Var1 = NA)
							for (l in 1:length(rvar)) {
								y = eval(parse(
										text=paste(
											dats[n], 
											"$", 
											rvar[l], 
											sep=""
										)
								))
								x = eval(parse(
										text = paste(
											dats[n], 
											"$",
											str_sub(rvar[l],-7,-1), 
											sep=""
										)
								))
								m = rep(1, length(y)) # equal P(inclusion) for all
								SampleRatio[[n]]$Var1 <- R_hat(
									y = y,
									x = x,
									N = N,
									n1 = n1,
									m = m
								)
							 	SampleRatio[[n]]$Var2 = var_R_hat(
							 		y = y, 
							 		x = x,
									N = N, 
							 		n1 = n1, 
							 		m = m
							 	)
								names(SampleRatio[[n]])[(dim(SampleRatio[[n]])[2] - 1) : 
									dim(SampleRatio[[n]])[2]] <- 
									c(
										paste(
											rvar[l], 
											"_ratio_mean_observed", 
											sep=""
										),
										paste(
											rvar[l], 
											"_ratio_var_observed", 
											sep=""
										)
									)
							}
							SampleRatio[[n]] %<>% mutate(Plots = dats[n])
						}
						SampleRatio <- do.call(rbind.data.frame, Ratio)
					}
				    SampleMeanVar %<>% merge(SampleRatio)	
				} else
				{
					alldata %<>% filter(Sampling!="Edge") %>% 
						as.data.table
				}
				if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
					################ HORVITZ-THOMPSON ESTIMATORS ###############
					HT_results <- list()
					alldata %<>% setkey(NetworkID)
					# OCCUPANCY AND ABUNDANCE
					# summarise data for mean calculations
					O <- alldata %>% 
						filter(Sampling!="Edge") %>%
						select_(.dots=c(oavar,"NetworkID","m")) %>%
						as.data.table
					# calculate y_HT
					m <- O$m
					if (y_HT_formula == "y_HT_RACS")
					{
						HT_results[[1]] <- O %>%
						.[, oavar, with=FALSE] %>% 
							.[, lapply(
								.SD,
								new_y_HT,
								N	= N, 
								n1	= n1,
								m	= m,
								m_threshold = m_threshold
							)]
					} else if (y_HT_formula == "y_HT")
					{
						HT_results[[1]] <- O %>%
							select_(.dots=oavar) %>%
							.[, lapply(
								.SD,
								y_HT,
								N	= N, 
								n1	= n1,
								m	= m
							)]
					}
					names(HT_results[[1]]) <- c(occ_abund_mean_names)
					# summarise data for variance calculations
					O_smd <- alldata %>% 
						.[, c(
							paste(oavar, "_network_sum", sep=""), 
							"NetworkID", 
							"m"
						), with=FALSE] %>% 
						filter(!(is.na(NetworkID))) %>%
						as.data.table %>%
						.[, lapply(.SD, function(x) {x[1]}), by=NetworkID]
					m <- O_smd$m
					# var_y_HT
					if (var_formula == "var_y_HT_RACS") {
						HT_results[[2]] <- O_smd[, paste(
							oavar, 
							"_network_sum", 
							sep=""
						), with=FALSE] %>%
							.[, lapply(
								.SD, 
								var_y_HT_RACS, 
								N 	= N, 
								n1 	= n1, 
								m	= m,
								m_threshold = m_threshold
							)]
						names(HT_results[[2]]) <- c(occ_abund_var_names)
					} else if (var_formula == "var_y_HT") {
						HT_results[[2]] <- O_smd[, paste(
							oavar, 
							"_network_sum", 
							sep=""
						), with=FALSE] %>%
							.[, lapply(
								.SD, 
								var_y_HT, 
								N 	= N, 
								n1 	= n1, 
								m	= m
							)]
						names(HT_results[[2]]) <- c(occ_abund_var_names)
						######################################################
					} else if (var_formula == "var_pi") {
						HT_results[[2]] <- O_smd[, paste(
							oavar, 
							"_network_sum", 
							sep=""
						), with=FALSE] %>%
							.[, lapply(
								.SD, 
								var_pi, 
								N 	= N, 
								n1 	= n1, 
								m	= m
							)]
						names(HT_results[[2]]) <- c(occ_abund_var_names)
					}
					# RATIO DATA
					if (!(is.null(rvar))) {
						# RATIO
						# summarise data for variance calculations
						# do I want to use summarised for everything??????????????
						mvals <- alldata %>%
							group_by(NetworkID) %>%
							summarise(m = m[1])
						R_smd <- alldata %>%
							filter(Sampling!="Edge") %>%
							as.data.table %>%
							.[, c(rvar, ovar, "NetworkID"), with=FALSE] %>%
							.[, lapply(.SD, sum, na.rm=T), by=NetworkID] %>%
							merge(mvals, by="NetworkID")
						# summarise data for mean calculations
						# R <- alldata %>% 
						#	filter(Sampling!="Edge") %>%
						#	.[, c(rvar, ovar, "m"), with=FALSE]
						HT_results[[3]] <- data.frame(Var1 = NA)
						for (l in 1:length(rvar)) {
							y = eval(parse(text=paste("R_smd$", rvar[l], 
								sep="")))
							x = eval(parse(text = paste("R_smd$", 
								str_sub(rvar[l],-7,-1), sep="")))
							HT_results[[3]]$Var1 = R_hat(
								y = y,
								x = x,
								N = N, 
								n1 = n1, 
								m = R_smd$m
							)
						 	HT_results[[3]]$Var2 = var_R_hat(
						 		y = y, 
						 		x = x,
								N = N, 
						 		n1 = n1, 
						 		m = R_smd$m
						 	)
							names(HT_results[[3]])[ 
								(dim(HT_results[[3]])[2] - 1) : 
								dim(HT_results[[3]])[2]
							] <- c(
									paste(
										rvar[l], 
										"_ratio_mean_observed", 
										sep=""
									),
									paste(
										rvar[l], 
										"_ratio_var_observed", 
										sep=""
									)
								)
						}
					}
					# merge together			
					All_HT <- HT_results %>% 
						as.data.frame %>%
						mutate(Plots = "Horvitz Thompson Mean (All Plots)")
					# merge estimates
					if (SampleEstimators == TRUE) {
						A[[i]][[j]][[k]] = rbind.fill(SampleMeanVar, All_HT)
					} else
					{
						A[[i]][[j]][[k]] <- All_HT
					}
				} else
				{
					A[[i]][[j]][[k]] <- SampleMeanVar
				}
				# add other information
				A[[i]][[j]][[k]]$simulation 		= k
				A[[i]][[j]][[k]]$seed 				= temp_seed
				A[[i]][[j]][[k]]$N.ACS.plots 		= dim(alldata_all)[1] - n1
				A[[i]][[j]][[k]]$N.Total.plots 		= dim(alldata_all)[1]
				A[[i]][[j]][[k]]$realization_variable = eval(parse(text=paste(
														"P$",
														realization_variable,
														sep=""
													)))[1]
				A[[i]][[j]][[k]]$patch_variable		= eval(parse(text=paste(
														"P$",
														patch_variable,
														sep=""
													)))[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	= n1
				# m characteristics
				if (mCharacteristics == TRUE) {
					if (sum(alldata_all$Cactus) > 0) {
						temp <- alldata_all[which(
							eval(parse(text=y_variable)) > 0
						),] 
						A[[i]][[j]][[k]]$mean_m <- mean(temp$m)
						A[[i]][[j]][[k]]$median_m <- median(temp$m)
						A[[i]][[j]][[k]]$max_m <- max(temp$m)
						A[[i]][[j]][[k]]$min_m <- min(temp$m)
						temp %<>%
							group_by(NetworkID) %>%
							summarise(m = m[1]) %>%
							summarise(
								MEAN = mean(m),
								MAX = max(m),
								MIN = min(m),
								MEDIAN = median(m)
							)
						A[[i]][[j]][[k]]$mean_unique_m <- temp$MEAN
						A[[i]][[j]][[k]]$median_unique_m <- temp$MEDIAN
						A[[i]][[j]][[k]]$max_unique_m <- temp$MAX
						A[[i]][[j]][[k]]$min_unique_m <- temp$MIN
					} else
					{
						A[[i]][[j]][[k]]$mean_m <- NA
						A[[i]][[j]][[k]]$median_m <- NA
						A[[i]][[j]][[k]]$max_m <- NA
						A[[i]][[j]][[k]]$min_m <- NA
						A[[i]][[j]][[k]]$mean_unique_m <- NA
						A[[i]][[j]][[k]]$median_unique_m <- NA
						A[[i]][[j]][[k]]$max_unique_m <- NA
						A[[i]][[j]][[k]]$min_unique_m <- NA
					}
				}
				# Spatial Statistics
				if (SpatialStatistics == TRUE) {
					if (sum(alldata_all$Cactus) > 1) {
						temp <- alldata_all %>%
							as.data.frame %>%
							# get rid of edge units - not involved in calculation of m
							filter(!(is.na(NetworkID))) %>%
							arrange(x, y)
							
							dnearneigh
							
							
						nb <- cell2nb(
							nrow = max(temp$x) - min(temp$x), 
							ncol = max(temp$y) - min(temp$y)
						)
						coordinates(temp) = ~ x+y
						data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
						if ("W" %in% weights) {
							lwb <- nb2listw(nb, style = "W") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.W <- 
								joincount.test(as.factor(temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.W <- moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}
						if ("B" %in% weights) {
							lwb <- nb2listw(nb, style = "B") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.B <- 
								joincount.test(as.factor(
								temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.B <- moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}	
						if ("C" %in% weights) {
							lwb <- nb2listw(nb, style = "C") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.C <- 
								joincount.test(as.factor(
								temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.C <- moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}	
						if ("U" %in% weights) {
							lwb <- nb2listw(nb, style = "U") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.U <- 
								joincount.test(as.factor(
								temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.U <- moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}	
						if ("S" %in% weights) {
							lwb <- nb2listw(nb, style = "S") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.S <- 
								joincount.test(as.factor(
								temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.S <- moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}	
						if ("minmax" %in% weights) {
							lwb <- nb2listw(nb, style = "minmax") # convert to weights
							# I think cells are indexed by row, then column
							A[[i]][[j]][[k]]$JoinCountTest.minmax <- 
								joincount.test(as.factor(
								temp$Cactus),
								lwb
							)[[2]]$estimate[1]
							A[[i]][[j]][[k]]$MoranI.minmax <- 
								moran.test(
								temp$Cactus,
								lwb
							)$estimate[1]
						}
					} else {
						if ("W" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.W <- NA
							A[[i]][[j]][[k]]$MoranI.W <- NA
						}
						if ("B" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.B <- NA
							A[[i]][[j]][[k]]$MoranI.B <- NA
						}	
						if ("C" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.C <- NA
							A[[i]][[j]][[k]]$MoranI.C <- NA
						}	
						if ("U" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.U <- NA
							A[[i]][[j]][[k]]$MoranI.U <- NA
						}	
						if ("S" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.S <- NA
							A[[i]][[j]][[k]]$MoranI.S <- NA
						}	
						if ("minmax" %in% weights) {
							A[[i]][[j]][[k]]$JoinCountTest.minmax <- NA
							A[[i]][[j]][[k]]$MoranI.minmax <- NA
						}	
					}
				}
			}
			do.call(rbind.data.frame, A[[i]][[j]])
	}
	Z$f_max 				= f_max
	Z$m_threshold 			= m_threshold
	Z$n_simulations 		= simulations
	Z$simulation_date 		= format(Sys.time(), "%m-%d-%y")
	Z$y_HT_formula 			= y_HT_formula
	Z$SamplingDesign 		= SamplingDesign
	Z$MoransIWeightMatrix 	= weights
	print(Sys.time() - TIME)
	return(Z)
}