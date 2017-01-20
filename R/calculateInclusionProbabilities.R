#' Calculate Inclusion Probabilities Using Simulations
#' @param patchdat patch realizations
#' @param simulations Number of simulations per population.
#' @param nsamples Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param SamplingDesign Sampling design; ACS or RACS.
#' @param y_variable variable upon which adaptive cluster sampling criterion is based
#' @description Calculate inclusion probabilities for each unit in a population using simulations.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @export

calculateInclusionProbabilities <- function(
	patchdat = patch_data_3, 
	simulations, 
	nsamples, 
	#ACS=TRUE, 
	SamplingDesign="ACS",
	y_variable,
	f_max = NULL
) 
{
	n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
	TIME 					<- Sys.time()
	patchdat 				%<>% arrange(n.networks, realization)
	n.patches 				<- length(unique(patchdat$n.networks))
	nsample.length 			<- length(nsamples)
	A 						<- vector("list", n.patches)
	B = foreach (
		i = 1:n.patches, # for each species density
		.inorder = FALSE, 
		.packages = c("magrittr", "foreach", "plyr", "dplyr", "data.table",
		 	"ACSampling", "intergraph", "network", "igraph", "stringr"), 
		.combine = "rbind.fill"
		) %:%
	 	foreach (
			j = 1:nsample.length, # for each sampling effort
			.combine = "rbind.fill",
			.inorder = FALSE
		) %dopar% {
			P 			<- patchdat %>% 
							filter(n.networks==unique(patchdat$n.networks)[i])
			N 			<- dim(P)[1]
			n1 			<- nsamples[j]
			A[[i]][[j]] <- list()
			r 			<- (i - 1) * j + j
			seeds 		<- runif(simulations)
		    for (k in 1:simulations) {
				temp_seed <- seeds[k]*100000
				if (SamplingDesign=="ACS") {
					alldata <- createACS(
						population=P, 
						seed=temp_seed, 
						n1=n1, 
						y_variable=y_variable
					) %>% 
						as.data.table
				} else {
					alldata <- createRACS_flex(
						population=P, 
						seed=temp_seed, 
						n1=n1, 
						y_variable=y_variable,
						f_max = f_max
					) %>% 
						as.data.table
				}
				A[[i]][[j]][[k]] <- alldata %>% 
					filter(Sampling!="Edge") %>%
					dplyr::select(n.networks, realization, x, y)
				cactus_networks <- alldata %>%
					#filter(!(Cactus==0 & m==1)) %>%
					filter(m!=0)
				if (dim(cactus_networks)[1] > 0) {
					temp <- cactus_networks %>%
						group_by(NetworkID) %>%
						summarise(m = m[1]) %>%
						summarise(
							MEAN = mean(m),
							MAX = max(m),
							MIN = min(m),
							MEDIAN = median(m)
						)
					A[[i]][[j]][[k]]$mean_m_unique_neigh 		<- temp$MEAN
					A[[i]][[j]][[k]]$max_m_unique_neigh 		<- temp$MAX
					A[[i]][[j]][[k]]$min_m_unique_neigh 		<- temp$MIN
					A[[i]][[j]][[k]]$median_m_unique_neigh 		<- temp$MEDIAN
					temp2 <- cactus_networks %>%
						summarise(
							MEAN = mean(m),
							MAX = max(m),
							MIN = min(m),
							MEDIAN = median(m)
						)
					A[[i]][[j]][[k]]$mean_m 	<- temp2$MEAN
					A[[i]][[j]][[k]]$max_m 		<- temp2$MAX
					A[[i]][[j]][[k]]$min_m 		<- temp2$MIN
					A[[i]][[j]][[k]]$median_m 	<- temp2$MEDIAN
					
					
					
							
				} else {
					A[[i]][[j]][[k]]$mean_m 	<- 0
					A[[i]][[j]][[k]]$max_m 		<- 0
					A[[i]][[j]][[k]]$min_m 		<- 0
					A[[i]][[j]][[k]]$median_m 	<- 0
				}
				A[[i]][[j]][[k]]$seed 				<- temp_seed
				A[[i]][[j]][[k]]$SamplingDesign 	<- SamplingDesign
				A[[i]][[j]][[k]]$simulations 		<- simulations
				A[[i]][[j]][[k]]$realization 		<- P$realization[1]
				A[[i]][[j]][[k]]$n.networks 		<- P$n.networks[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	<- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			X$coords <- with(X, paste(x,y,sep="_"))
			# total number of cells sample per realization, n.networks, N.SRSWOR.plots and SamplingDesign
			X %>%
				group_by(
					realization, 
					n.networks, 
					N.SRSWOR.plots, 
					SamplingDesign,
					simulations,
					coords,
					mean_m,
					max_m,
					min_m,
					median_m
				) %>%
				summarise(times_included=n())	
	}
	B$simulation_date 	= format(Sys.time(), "%m-%d-%y")
	B$f_max 		= f_max
	print(Sys.time() - TIME)
	return(B)
}