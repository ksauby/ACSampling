#' Sample cactus patch realizations
#' @param patch_data
#' @param simulations Number of simulations per population.
#' @param nsamples Vector of initial sample sizes (sampled according to SRSWOR) to simulate.
#' @param population The extent of the sampling area, represented as a grid of locations with coordinates (x, y).
#' @param abundance.variables Vector of variables for which abundance should be estimated.
#' @param occupancy.variables Vector of variables for which occupancy should be estimated.
#' @param seed.generator A number that will be used to determined the starting value of set.seed() for the set of simulations for each group.
#' @param ACS Whether adaptive cluster sampling should be performed; defaults to \code{TRUE}.
#' @param Restricted Whether restricted or unrestricted adaptive cluster sampling should be performed; defaults to \code{FALSE}.
#' @description This function simulates sampling of multiple realizations of patches of the species of interest within the grid of locations created with \code{createPopulation}.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples
#' simulations=200
#' nsamples=c(5,10,20,40)
#' population <- createPopulation(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
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
#' simulation_data <- sampleSpeciesPatchRealizations(patch_data, simulations, nsamples, population, abundance.variables, occupancy.variables)
#' simulations=200
#' #nsamples=c(75,150,225,300,350)
#' simulation_data_SRSWOR <- sampleSpeciesPatchRealizations(patch_data, simulations, nsamples, population, abundance.variables, occupancy.variables)
#' # save data
#' write.csv(simulation_data_SRSWOR, file=paste("simulation_data_SRSWOR", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".csv", sep=""))

sampleSpeciesPatchRealizationsRestricted <- function(patch_data, simulations, nsamples, population, abundance.variables, occupancy.variables, seed.generator=1000, ACS=TRUE) {
	x = Sys.time()
	variables = c(occupancy.variables, abundance.variables)
	data.array <- list()
	# for each species density
	Z = foreach (i=1:length(unique(patch_data$n.networks)), .inorder=FALSE, 
		.packages="ACSampling", .combine="rbind") %dopar% {
		patch = patch_data %>% filter(n.networks==unique(patch_data$n.networks)[i])
		N = dim(patch_data)[1]
		# for each sampling effort
	 	foreach (j=1:length(nsamples), .combine="rbind", .inorder=FALSE) %dopar% {
			seed = seq(j*i*seed.generator, j*i*seed.generator+999, by=1) 
			n1 = nsamples[j]
  			data.array[[i]] <- list()
			data.array[[i]][[j]] <- list()
			# for each simulation
		    for (k in 1:simulations) {
				# data.array[[i]][[j]][[k]] <- list()
				temp_seed = seed[1]
				seed = seed[-1]
				#if (ACS==TRUE) {
					alldata = createRestrictedACS(patch, temp_seed, n1, "Cactus") %>% 
						as.data.table
					SRSWOR_data = alldata %>% filter(Sampling=="SRSWOR")
					# create dataframes to save summary statistics
					All_HT <- data.frame(Plots="Horvitz Thompson Mean (All Plots)")
					# estimates for each variable
						# SRSWOR estimates
							SRSWOR <- SRSWOR_data[, variables, with=FALSE] %>% 
								summarise_each(
									funs(
										mean(., na.rm=T), 
										var(., na.rm=T), 
										sum(., na.rm=T)
									)
								)
							setnames(SRSWOR, names(SRSWOR), paste(names(SRSWOR), "observed", sep="_"))
							SRSWOR$Plots="Mean of SRSWOR Plots"
						# estimates using all data, ignoring sampling scheme
							All <- alldata[, variables, with=FALSE] %>%
								summarise_each(
									funs(
										mean(., na.rm=T), 
										var(., na.rm=T), 
										sum(., na.rm=T)
									)
								)
							setnames(All, names(All), paste(names(All), "observed", sep="_"))
							All$Plots="Simple Mean (All Plots)"
					# HT estimators
					for (l in 1:length(variables)) {
						if (variables[l] %in% occupancy.variables) {
							summary = alldata %>%
								filter(Sampling!="Edge") %>%
								group_by(NetworkID) %>%
								summarise(
									m = m[1],
									# occupancy
									y_value = ifelse(sum(eval(parse(text = variables[l])),
										na.rm=T) > 0,1, 0)
								)
						}
						else 
						{
							summary = alldata %>%
								filter(Sampling!="Edge") %>%
								group_by(NetworkID) %>%
								summarise(
									m = m[1],
									# abundance
									y_value = sum(eval(parse(text = variables[l])), na.rm=T)
								)
						}
						All_HT %<>%
							mutate(
								t_HT(
									N = N,
									n1 = n1,
									mk = alldata$m,
									y = eval(parse(text=paste("alldata$", variables[l], sep=""))),
									sampling = alldata$Sampling, criterion = 0
								),
								var_t_HT(
									N = N, 
									n1 = n1, 
									m = summary$m, 
									y = summary$y_value
								)
							)
						# names
						names(All_HT)[(dim(All_HT)[2] - 1) : dim(All_HT)[2]] <- c(
							paste(variables[l], "_mean_observed", sep=""),
							paste(variables[l], "_var_observed", sep="")
						)
					}
					# merge estimates
					data.array[[i]][[j]][[k]] = rbind.fill(SRSWOR, All, All_HT) %>%
					# add other information
						mutate(
							simulation = k,
							seed = temp_seed,
							N.ACS.plots = dim(alldata)[1] - n1,
							N.Total.plots = dim(alldata)[1]
						)
			}
			do.call(rbind.data.frame, data.array[[i]][[j]]) %>%
				mutate(
					realization = patch$realization[1],
					n.networks = patch$n.networks[1],
					N.SRSWOR.plots = n1
				)
		}
	}
	print(Sys.time() - x)
	return(Z)
}