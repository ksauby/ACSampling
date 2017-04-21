#' Calculate Joint Inclusion Probabilities Using Simulations
#' @param patchdat patch realizations
#' @param simulations Number of simulations per population.
#' @param nsamples Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param SamplingDesign Sampling design; ACS or RACS.
#' @param y_variable variable upon which adaptive cluster sampling criterion is based
#' population.grouping.variable variable identifying unique populations
#' @description Calculate inclusion probabilities for each unit in a population using simulations.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @export

calculateJointInclusionProbabilities <- function(
	patchdat = patch_data_3, 
	simulations, 
	nsamples, 
	#ACS=TRUE, 
	SamplingDesign="ACS",
	y_variable,
	f_max = NULL,
	population.grouping.variable=NULL
) 
{
	pop <- i <- j <- Sampling <- . <- NetworkID <- NULL
	TIME 			<- Sys.time()
	patchdat 		%<>% arrange_(.dots=population.grouping.variable)
	if (population.grouping.variable != "pop") {
		patchdat	%<>% setnames(population.grouping.variable, "pop") 
	}
	n.pop 			<- length(unique(patchdat$pop))
	nsample.length 	<- length(nsamples)
	A 				<- vector("list", n.pop)
	B 				<- vector("list", n.pop)
	C = foreach (
		i = 1:n.pop, # for each species density
		.inorder = FALSE, 
		.packages = c("magrittr", "foreach", "plyr", "dplyr", "data.table",
		 	"ACSampling", "intergraph", "network", "igraph", "stringr"), 
		.combine = "list",
		.multicombine = TRUE
		) %:%
	 	foreach (
			j = 1:nsample.length, # for each sampling effort
			.multicombine = TRUE,
			.inorder = FALSE#,
			#.combine = function(X, Y) {
			# 	Xnew1 = rbind.fill(X)
			#	Ynew1 = rbind.fill(Y)
			#	list(Xnew1, Ynew1)
			#}
		) %dopar% {
			P 			<- patchdat %>% filter(pop==unique(patchdat$pop)[i])
			P$coords <- with(P, paste(x,y,sep="_"))
			N 			<- dim(P)[1]
			n1 			<- nsamples[j]
			A[[i]][[j]] <- list()
			B[[i]][[j]] <- list()
			r 			<- (i - 1) * j + j
			seeds 		<- runif(simulations)
			pop.matrix <- matrix(
				nrow=length(unique(P$NetworkID)),
				ncol=length(unique(P$NetworkID)),
				dimnames=list(
					unique(P$NetworkID),
					unique(P$NetworkID)
				),
				0
			)
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
					dplyr::select(pop, x, y)
				data_networks <- alldata %>%
					#filter(!(Cactus==0 & m==1)) %>%
					filter(m!=0)
				# GET INCLUSION MATRIX FOR NETWORKS
				A[[i]][[j]][[k]]$coords <- with(
					A[[i]][[j]][[k]], 
					paste(x,y,sep="_")
				)
				A[[i]][[j]][[k]] <- P %>% 
					dplyr::select(coords, NetworkID) %>%
					merge(A[[i]][[j]][[k]], by="coords")
				Z <- matrix(
					nrow=length(unique(A[[i]][[j]][[k]]$NetworkID)),
					ncol=length(unique(A[[i]][[j]][[k]]$NetworkID)),
					dimnames=list(
						unique(A[[i]][[j]][[k]]$NetworkID),
						unique(A[[i]][[j]][[k]]$NetworkID)
					),
					1
				)
				indxB <- outer(
					rownames(pop.matrix), 
					colnames(pop.matrix), 
					FUN=paste
				) %in% outer(
					rownames(Z), 
					colnames(Z), 
					FUN=paste
				)
				B1 <- pop.matrix
				B1[indxB] <- Z
				B[[i]][[j]][[k]] <- B1
				# SUMMARIZE M INFORMATION
				if (dim(data_networks)[1] > 0) {
					temp <- data_networks %>%
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
					temp2 <- data_networks %>%
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
				A[[i]][[j]][[k]]$sample 			<- k
				A[[i]][[j]][[k]]$seed 				<- temp_seed
				A[[i]][[j]][[k]]$SamplingDesign 	<- SamplingDesign
				A[[i]][[j]][[k]]$simulations 		<- simulations
				A[[i]][[j]][[k]]$pop 		<- P$pop[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	<- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			# X %>% 
			#	group_by(sample,NetworkID) %>%
			#	summarise(n())
			# total number of cells sample per population, N.SRSWOR.plots and SamplingDesign
			X %<>%
				group_by(
					pop, 
					N.SRSWOR.plots, 
					SamplingDesign,
					simulations,
					coords,
					mean_m,
					max_m,
					min_m,
					median_m
				) %>%
				summarise(times_included=n()) %>%
				as.data.frame
			Y <- Reduce("+", B[[i]][[j]])
			XY <- list(X, Y)
			names(XY) <- 
			c(
				"dat",
				paste(
					"nsamples",
					nsamples[j],
					"pop",
					unique(patchdat$pop)[i],
					sep="_"
				)
			)
			XY
	}
	
	k=1
	newlist <- list()
	for (i in 1:n.pop) {
		for (j in 1:nsample.length) {
			newlist[[k]] <- C[[i]][[j]][grep("nsamples", names(C[[i]][[j]]))]
			k <- k+1
		}
	}
	k=1
	newlist2 <- list()
	for (i in 1:n.pop) {
		for (j in 1:nsample.length) {
			newlist2[[k]] <- C[[i]][[j]][grep("dat", names(C[[i]][[j]]))]
			k <- k+1
		}
	}
	#newlist2 <- do.call(rbind.data.fill, newlist2)
	
	
	
#	Z <- list.map(C[[1]], dat)
	#A_1 <- lapply(C, `[[`, 1) %>% do.call(rbind.data.frame, .)
	#A_2 <- lapply(C, `[[`, 2)
	#C <- list(A_1, A_2)
#	C[[3]]$simulation_date 	= format(Sys.time(), "%m-%d-%y")
#	C[[4]]$f_max 		= f_max
	print(Sys.time() - TIME)
	return(newlist2)
}