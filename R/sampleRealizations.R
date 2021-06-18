#' Sample species patch realizations simulations

#' @param yvar variable upon which adaptive cluster sampling criterion is based
#' @param popdata patch realizations
#' @param sims Number of simulations per population.
#' @param n1 Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param avar Vector of variables for which abundance should be estimated. The total vector of variables (\code{c(avar, ovar, rvar)}) should be a length of at least 1.
#' @param ovar Vector of variables for which occupancy should be estimated. The total vector of variables (\code{c(avar, ovar, rvar)}) should be a length of at least 1.
#' @param rvar Vector of variables for which ratio estimators should be used. The total vector of variables (\code{c(avar, ovar, rvar)}) should be a length of at least 1.
#' @param SamplingDesign Whether simple random sampling (SRS), unrestricted adaptive cluster sampling ("ACS"), or restricted ACS ("RACS") should be performed; defaults to \code{ACS}.
#' @param y_HT_formula The formula used to estimate the population total: either the Horvitz-Thompson estimator, 'y_HT,' or or the RACS-corrected Horvitz-Thompson estimator, 'y_HT_RACS'.
#' @param var_formula The formula used to estimate the variance of the population total: either the Horvitz-Thompson variance estimator, 'var_y_HT', or the RACS-corrected Horvitz-Thompson variance estimator, "var_y_HT_RACS." Defaults to "var_y_HT".
#' @param mThreshold Default is NULL. OPTIONS
#' @param f_max Default is 2. OPTIONS
#' @param SampleEstimators If "TRUE", calculate the sample mean and sample variance for each simulation. Default is FALSE.
#' @param SpatStat TRUE or FALSE. If "TRUE", for each simulation calculate Moran's I, and the nugget, sill, and range of the semivariogram. Default is TRUE.
#' @param weights TRUE or FALSE. If SpatStat is "TRUE", this is a vector giving spatial weight matrix styles to use to calculate the Join Count and Moran's I statistics. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.
#' @param mChar TRUE or FALSE. If "TRUE", for each simulation calculate summary statistics (median, mean, min, and max) for the sample's m values. Also, for each simulation and for the set of unique m values, calculate the same summary statistics. If "FALSE," no summary statistics are calculated.
#' @param popvar Default is "n.networks"
#' @param realvar Default is "realization"

#' @description This function simulates sampling of multiple realizations of patches of the species of interest within the grid of locations created with \code{createPop}.

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @importFrom foreach foreach %dopar%
#' @importFrom dplyr summarise_all arrange_at row_number
#' @importFrom rlang sym syms
#' @importFrom stringr str_replace
#' @import sp
 
#' @export
#' 
#' #' @examples
sims=20
n1=c(5,10,20,40)
population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
#' avar = NULL
ovar = c(
	"Stricta",
	"Pusilla",
"Cactus",
"CACA_on_Pusilla",
"CACA_on_Stricta",
"MEPR_on_Pusilla",
"MEPR_on_Stricta",
"Old_Moth_Evidence_Pusilla",
"Old_Moth_Evidence_Stricta"
)
data(CactusRealizations)
popdata = CactusRealizations # WHY IS THERE ISLAND=NA
simulation_data <- sampleRealizations(
popdata = popdata,
	sims = sims,
	n1 = n1,
	avar = avar,
	ovar = ovar,
popvar="Island",
yvar="Cactus"
)
# sims=200# #n1=c(75,150,225,300,350)# simulation_data_SRSWOR <- sampleRealizations(# 	popdata = popdata,# 	sims = sims,# 	n1 = n1,# 	avar = avar,# 	ovar = ovar,# 	popvar="Island"# )
sampleRealizations <- function(
	popdata, 
	sims, 
	n1, 
	avar=NULL, 
	ovar, 
	rvar=NULL,
	#ACS=TRUE, 
	SamplingDesign="ACS",
	yvar,
	y_HT_formula = "y_HT",
	var_formula = "var_y_HT",
	mThreshold = NULL,
	f_max = 2,
	SampleEstimators = FALSE,
	SpatStat = TRUE,
	mChar = TRUE,
	popvar = "n.networks",
	realvar = "realization",
	weights="S"
) 
{
     handleError_popdata(popdata)
     handleError_n1vector(n1)
     handleError_yvar(yvar)
     handleError_LogicalVar(SampleEstimators, "SampleEstimators")
     handleError_LogicalVar(SpatStat, "SpatStat")
     handleError_LogicalVar(mChar, "mChar")
     
     vars <- c(ovar, avar, rvar)
     handleError_vars(vars)
     

     if (!(SamplingDesign %in% c("ACS", "RACS", "SRS"))) {
          stop("SamplingDesign must be supplied as either 'SRS', ACS', or 'RACS'.")
     }
     if (!(y_HT_formula %in% c("y_HT", "y_HT_RACS"))) {
          stop("y_HT_formula must be supplied as either 'y_HT' or 'y_HT_RACS'.")
     }
     if (!(var_formula %in% c("var_y_HT", "var_y_HT_RACS"))) {
          stop("var_formula must be supplied as either 'var_y_HT' or 'var_y_HT_RACS'.")
     }
     if (!(weights %in% c("W", "B", "C", "U", "S"))) {
          stop("weights must be supplied as 'W', 'B', 'C', 'U', or 'S'.")
     }

     if (is.character(popvar)==FALSE) {
          stop("The argument popvar must be a character string.")
     }
     if (is.character(realvar)==FALSE) {
          stop("The argument realvar must be a character string.")
     }
     
     POPVAR <- sym(popvar)
     REALVAR <- sym(realvar)
     n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
     TIME <- Sys.time()
     popdata %<>% arrange_at(c(popvar, realvar))
     n.patches <- length(unique(eval(parse(text=paste(
          "popdata$", popvar, sep="")))))
     nsample.length <- length(n1)
     A <- vector("list", n.patches)
     # c() - same code calculates the HT estimators for occupancy and abundance
     oavar <- c(ovar, avar)
     OAVAR <- syms(oavar)
     # empty dataframes will be cbind'd together after HT estimators calculated
     occ_abund_var <- data.frame(row.names = 1:length(c(ovar, avar))) 
     occ_abund_mean <- data.frame(row.names = 1:length(c(ovar, avar)))
     Ratio <- data.frame(row.names = 1:length(rvar)) 
	# the names to assign the estimates
	# occ_abund_mean_names <- paste(ovar, avar, "MeanObs", sep="")
	#occ_abund_var_names <- paste(ovar, avar, "VarObs", sep="")
	ratio_mean_names <- paste(rvar, "RMeanObs", sep="")
	ratio_var_names <- paste(rvar, "RVarObs", sep="")
	# i=1;j=1;k=1
	Z = foreach (
		i = 1:n.patches, # for each species density
		.inorder = FALSE, 
		.packages = c("magrittr", "foreach", "plyr", "dplyr", "data.table",
		 	"ACSampling", "intergraph", "network", "igraph", "stringr", 
		 	"spdep"), 
		.combine = "bind_rows",
		#.errorhandling = "pass",
		.verbose = TRUE
		) %:%
	 	foreach (
			j = 1:nsample.length, # for each sampling effort
			.combine = "bind_rows",
			.inorder = FALSE
		) %dopar% {
			cat(paste(i,j, sep="_"))
			P <- popdata %>% 
				filter(!!POPVAR == unique(
					eval(parse(text=paste("popdata$", popvar, sep="")))
				)[i])
			N <- dim(P)[1]
			n1 <- n1[j]
			A[[i]][[j]] <- list()
			r <- (i - 1) * j + j
			seeds <- runif(sims)
		    for (k in 1:sims) {
				temp_seed <- seeds[k]*100000
				if (SamplingDesign=="ACS") {
					alldata <- createACS(
						popdata=P, seed=temp_seed, n1=n1, yvar=yvar)
				} else if (SamplingDesign=="RACS") {
					alldata <- createRACS(
						popdata=P, seed=temp_seed, n1=n1, yvar=yvar, f_max=f_max)
				} else {
					alldata <- createSRS(
						popdata=P, seed=temp_seed, n1=n1)
				}
				alldata_all <- alldata
				if (SampleEstimators == TRUE) {
					################ SRSWOR Sampling #####################
					if (SamplingDesign!="ACS" & SamplingDesign!="RACS") {
						# datasets to apply simple mean/variance and 
					     #    simple ratio estimator
						dats <- "alldata"
					}
				     ################ SRSWOR Data, alldata ################
					if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
						SRSWOR_data <- alldata %>% 
							filter(Sampling=="SRSWOR")
						alldata %<>% filter(Sampling!="Edge")
						# apply simple mean/variance & simple ratio 
						#    estimator to:
						dats <- c("SRSWOR_data", "alldata")
					}
					SampleMeanVar <- list()
					for (n in 1:length(dats)) {
						dat <- eval(parse(text=dats[[n]])) %>%
							select(!!!OAVAR) %>%
							summarise_all(
							     list(mean), 
							     na.rm=T)
						names(dat) <- str_replace(
						     names(dat), "(.*)", "\\1_obs")
						dat$Plots <- dats[n]
						SampleMeanVar[[n]] <- dat
					}
					SampleMeanVar %<>% bind_rows
					# simple ratio estimators applied to alldata, 
					#    SRSWOR_data
					if (!(is.null(rvar))) {
						SmpRatio <- list()
						for (n in 1:length(dats)) {
							SmpRatio[[n]] <- data.frame(Var1 = NA)
							for (l in 1:length(rvar)) {
								y = eval(parse(text=paste(
									dats[n], "$", rvar[l], sep="")))
								x = eval(parse(text = paste(
									dats[n], "$", 
									str_sub(rvar[l],-7,-1), sep="")))
								# equal P(inclusion) for all
								m = rep(1, length(y))
								SmpRatio[[n]]$Var1 <- R_hat(
									y = y, x = x, N = N, n1 = n1, 
									m = m)
							 	SmpRatio[[n]]$Var2 = var_R_hat(
							 		y = y, x = x, N = N, n1 = n1, 
							 		m = m)
								names(SmpRatio[[n]])[(dim(SmpRatio[[n]])[2]-1): 
									dim(SmpRatio[[n]])[2]] <- 
									c(
										paste(rvar[l], "RMeanObs", 
											sep=""),
										paste(rvar[l], "RVarObs", 
											sep="")
									)
							}
							SmpRatio[[n]] %<>% mutate(Plots = dats[n])
						}
						SmpRatio <- do.call(rbind.data.frame, Ratio)
						SampleMeanVar %<>% merge(SmpRatio)	
					}
				} else
				{
					alldata %<>% filter(Sampling!="Edge")
				}
				if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
					################ HORVITZ-THOMPSON ESTIMATORS ##########
					HT_results <- list()
					# OCCUPANCY AND ABUNDANCE
					# summarise data for mean calculations
					O <- alldata %>% 
						filter(Sampling!="Edge") %>%
						select(!!!OAVAR, NetworkID, m)
					# calculate y_HT
					m <- O$m
					if (y_HT_formula == "y_HT_RACS") {
						HT_results[[1]] <- calcyHTMultipleVars(O, N, n1, m, mThreshold)
						     
						     calcyHTMultipleVars <- function(O, N, n1, m, mThreshold) {
						          O %>%
							dplyr::select(!!!OAVAR) %>%
							summarise_all(
								list(yHT = new_y_HT),
								N = N, n1 = n1, m = m, 
								m_threshold = mThreshold
							)
					} else if (y_HT_formula == "y_HT") {
						HT_results[[1]] <- O %>%
							dplyr::select(!!!OAVAR) %>%
							summarise_all(
								list(yHT = y_HT),
								N = N, n1 = n1, m = m
							)
					}
					# summarise data for variance calculations
					O_smd <- alldata %>% 
						select(!!!OAVAR, NetworkID, m) %>%
						filter(!(is.na(NetworkID))) %>%
						group_by(NetworkID) %>%
						filter(row_number()==1) %>%
					     ungroup()
					m <- O_smd$m
					# var_y_HT
					if (var_formula == "var_y_HT_RACS") {
						HT_results[[2]] <- O_smd %>% 
							select(!!!OAVAR) %>%
							summarise_all(
								list(
								     var_yHT_RACS = var_y_HT_RACS
								),
								N=N,  n1=n1,  m=m, m_threshold=2
							)
					} else if (var_formula == "var_y_HT") {
						HT_results[[2]] <- O_smd %>% 
							select(!!!OAVAR) %>%
							summarise_all(
								list(var_yHT=var_y_HT),
								N=N, n1=n1, m=m
							)
					######################################################
					} else if (var_formula == "var_pi") {
						HT_results[[2]] <- O_smd %>% 
							select(!!!OAVAR) %>%
							summarise_all(
								list(var_pi = var_pi),
								N=N, n1=n1, m=m
							)
					}
					# RATIO DATA
					if (!(is.null(rvar))) {
						rovar <- c(rvar, ovar)
						ROVAR <- syms(rovar)
						# RATIO
						# summarise data for variance calculations
						mvals <- alldata %>%
							group_by(NetworkID) %>%
							summarise(m = m[1])
						R_smd <- alldata %>%
							filter(Sampling!="Edge") %>%
							select(!!!ROVAR, "NetworkID") %>%
							group_by(NetworkID) %>%
							#as.data.table %>%
							summarise_all(
								list(sum = sum),
								na.rm = T
							) %>%
							merge(mvals, by="NetworkID")
						# summarise data for mean calculations
						HT_results[[3]] <- data.frame(Var1 = NA)
						for (l in 1:length(rvar)) {
							y = eval(parse(text=paste("R_smd$", rvar[l], 
								sep="")))
							x = eval(parse(text = paste("R_smd$",
							# GENERALIZE THIS 
								str_sub(rvar[l],-7,-1), sep="")))
							HT_results[[3]]$Var1 = R_hat(
								y = y, x = x, N = N, n1 = n1, 
								m = R_smd$m)
						 	HT_results[[3]]$Var2 = var_R_hat(
						 		y=y, x=x, N=N, n1=n1, m=R_smd$m)
							names(HT_results[[3]])[ 
								(dim(HT_results[[3]])[2] - 1) : 
								dim(HT_results[[3]])[2]
							] <- c(
									paste(rvar[l], "RMeanObs", sep=""),
									paste(rvar[l], "RVarObs", sep="")
								)
						}
					}
					# merge together			
					All_HT <- HT_results %>% 
						as.data.frame %>%
						mutate(Plots = "Horvitz Thompson Mean (All Plots)")
					# merge estimates
					if (SampleEstimators == TRUE) {
						A[[i]][[j]][[k]] = bind_rows(SampleMeanVar, All_HT)
					} else
					{
						A[[i]][[j]][[k]] <- All_HT
					}
				} else
				{
					A[[i]][[j]][[k]] <- SampleMeanVar
				}
				# add other information
				A[[i]][[j]][[k]]$simulation = k
				A[[i]][[j]][[k]]$seed = temp_seed
				A[[i]][[j]][[k]]$N.ACS.plots = dim(alldata_all)[1] - n1
				A[[i]][[j]][[k]]$N.Total.plots = dim(alldata_all)[1]
				A[[i]][[j]][[k]]$realvar = eval(parse(text=paste(
					"P$", realvar, sep="")))[1]
				A[[i]][[j]][[k]]$popvar = eval(parse(text=paste(
					"P$", popvar, sep="")))[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots = n1
				# m characteristics
				if (mChar == TRUE) {
					if (sum(alldata_all$Cactus) > 0) {
						temp <- alldata_all[which(
							eval(parse(text=yvar)) > 0
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
						A[[i]][[j]][[k]]$mean_uniq_m <- temp$MEAN
						A[[i]][[j]][[k]]$median_uniq_m <- temp$MEDIAN
						A[[i]][[j]][[k]]$max_uniq_m <- temp$MAX
						A[[i]][[j]][[k]]$min_uniq_m <- temp$MIN
					} else
					{
						A[[i]][[j]][[k]]$mean_m <- NA
						A[[i]][[j]][[k]]$median_m <- NA
						A[[i]][[j]][[k]]$max_m <- NA
						A[[i]][[j]][[k]]$min_m <- NA
						A[[i]][[j]][[k]]$mean_uniq_m <- NA
						A[[i]][[j]][[k]]$median_uniq_m <- NA
						A[[i]][[j]][[k]]$max_uniq_m <- NA
						A[[i]][[j]][[k]]$min_uniq_m <- NA
					}
				}
				# Spatial Statistics
				if (SpatStat == TRUE) {
					if (sum(alldata_all$Cactus) > 1) {
						temp <- alldata_all %>%
							as.data.frame %>%
							# get rid of edge units - not involved in calculation of m
							filter(!(is.na(NetworkID))) %>%
							arrange(x, y)
							
							# dnearneigh - why was this here?
							
							
						nb <- cell2nb(
							nrow = max(temp$x) - min(temp$x), 
							ncol = max(temp$y) - min(temp$y)
						)
						coordinates(temp) = ~ x+y
						data_dist <- dim(
							as.matrix(dist(cbind(temp$x, temp$y)))
						)[1]
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
	Z$f_max 		= f_max
	Z$mThreshold 	= mThreshold
	Z$nSims			= sims
	Z$SimDate 		= format(Sys.time(), "%m-%d-%y")
	Z$y_HT_formula 	= y_HT_formula
	Z$SmplngDsgn 	= SamplingDesign
	Z$MrnsIWghtMtrx = weights
	print(Sys.time() - TIME)
	return(Z)
}