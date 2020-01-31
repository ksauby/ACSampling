#' Calculate Inclusion Probabilities Using Simulations

#' @param popdata Population data
#' @param sims Number of simulations per population.
#' @param nsamples Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param f_max NEED DESCRIPTION HERE
#' @param SamplingDesign Adaptive cluster sampling design (either "ACS" or "RACS").
#' @param y_variable Variable upon which adaptive cluster sampling criterion is based

#' @description Calculate inclusion probabilities for each unit in a realization using simulations.

#' @value Returns a dataframe with the following columns:
#' realization
#' n.networks
#' N.SRSWOR.plots
#' SamplingDesign
#' simulations
#' coords
#' mean_m
#' max_m
#' min_m
#' median_m


#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @importFrom stats runif median
#' @importFrom dplyr summarise

#' @export
	
estim_pi_i <- function(
	popdata, 
	sims, 
	nsamples, 
	SamplingDesign="ACS",
	y_variable,
	f_max = NULL
) 
{
	n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
	TIME 					<- Sys.time()
	popdata 				%<>% arrange(.data$n.networks, .data$realization)
	n.patches 				<- length(unique(popdata$n.networks))
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
			P 			<- popdata %>% 
							filter(.data$n.networks==unique(popdata$n.networks)[i])
			N 			<- dim(P)[1]
			n1 			<- nsamples[j]
			A[[i]][[j]] <- list()
			r 			<- (i - 1) * j + j
			seeds 		<- runif(sims)
		    for (k in 1:sims) {
				temp_seed <- seeds[k]*100000
				if (SamplingDesign=="ACS") {
					alldata <- createACS(
						popdata=P, 
						seed=temp_seed, 
						n1=n1, 
						yvar=y_variable
					)
				} else {
					alldata <- createRACS(
						popdata=P, 
						seed=temp_seed, 
						n1=n1, 
						yvar=y_variable,
						f_max = f_max
					)
				}
				A[[i]][[j]][[k]] <- alldata %>% 
					filter(.data$Sampling!="Edge") %>%
					select(.data$n.networks, .data$realization, .data$x, .data$y)
				cactus_networks <- alldata %>%
					#filter(!(Cactus==0 & m==1)) %>%
					filter(.data$m!=0)
				if (dim(cactus_networks)[1] > 0) {
					temp <- cactus_networks %>%
						group_by(.data$NetworkID) %>%
						summarise(m = .data$m[1]) %>%
						summarise(
							MEAN = mean(.data$m),
							MAX = max(.data$m),
							MIN = min(.data$m),
							MEDIAN = median(.data$m)
						)
					A[[i]][[j]][[k]]$mean_m_unique_neigh 		<- temp$MEAN
					A[[i]][[j]][[k]]$max_m_unique_neigh 		<- temp$MAX
					A[[i]][[j]][[k]]$min_m_unique_neigh 		<- temp$MIN
					A[[i]][[j]][[k]]$median_m_unique_neigh 		<- temp$MEDIAN
					temp2 <- cactus_networks %>%
						summarise(
							MEAN = mean(.data$m),
							MAX = max(.data$m),
							MIN = min(.data$m),
							MEDIAN = median(.data$m)
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
				A[[i]][[j]][[k]]$sims 		<- sims
				A[[i]][[j]][[k]]$realization 		<- P$realization[1]
				A[[i]][[j]][[k]]$n.networks 		<- P$n.networks[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	<- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			X$coords <- with(X, paste(x,y,sep="_"))
			# total number of cells sample per realization, n.networks, N.SRSWOR.plots and SamplingDesign
			X %>%
				group_by(
					.data$realization, 
					.data$n.networks, 
					.data$N.SRSWOR.plots, 
					.data$SamplingDesign,
					.data$sims,
					.data$coords,
					.data$mean_m,
					.data$max_m,
					.data$min_m,
					.data$median_m
				) %>%
				summarise(times_included=n())	
	}
	B$simulation_date 	= format(Sys.time(), "%m-%d-%y")
	B$f_max 		= f_max
	print(Sys.time() - TIME)
	return(B)
}