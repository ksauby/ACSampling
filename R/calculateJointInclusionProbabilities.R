#' Calculate Joint Inclusion Probabilities Using Simulations

#' @param patchdat A data frame specifying the patch realizations. WHAT FORMAT SHOULD IT BE IN?
#' @param simulations Number of simulations per population.
#' @param nsamples Vector of initial sample size(s) for the initial simple random sample(s) without replacement; can be a single value or vector of values
#' @param SamplingDesign Sampling design; ACS or RACS. Default value is "ACS."
#' @param y_variable variable upon which adaptive cluster sampling criterion is based
#' @param f_max Default value is NULL.
#' @param population.grouping.variable variable identifying unique populations. Default value is NULL.

#' @description Calculate inclusion probabilities for each unit in a population using simulations.

#' @return A list of nsamples and dat, BUT I DONT REMEMBER WHAT THOSE ARE.

#' @export

calculateJointInclusionProbabilities <- function(
	patchdat, 
	simulations, 
	nsamples, 
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
		colnames(patchdat)[which(names(patchdat) == population.grouping.variable)] <- "pop"
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
			N 			<- dim(P)[1]
			n1 			<- nsamples[j]
			# save simulation attributes
			A[[i]][[j]] <- list()
			# save counts of joint inclusions
			B[[i]][[j]] <- matrix(
				nrow=length(unique(P$NetworkID)),
				ncol=length(unique(P$NetworkID)),
				dimnames=list(
					unique(P$NetworkID),
					unique(P$NetworkID)
				),
				0
			)
			# zeros
			zeros <- B[[i]][[j]]
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
					dplyr::select(pop, x, y, NetworkID, Sampling) %>%
					arrange(NetworkID)
				# save for m summary data
				data_networks <- alldata %>%
					filter(m!=0)
				# GET INCLUSION MATRIX FOR NETWORKS
				
				# temp <- 
				# 	A[[i]][[j]][[k]] %>%
				# 	filter(Sampling=="SRSWOR") %>%
				# 	group_by(NetworkID) %>%
				# 	dplyr::summarise(n()) %>%
				# 	arrange(-`n()`)
				# Z <- matrix(
				# 	temp$`n()`, 
				# 	nrow=dim(temp)[1], 
				# 	ncol=dim(temp)[1], 
				# 	dimnames=list(temp$NetworkID, temp$NetworkID)
				# )
				#Z[lower.tri(Z)] <- t(Z)[lower.tri(Z)]
				
				Z <- matrix(
					nrow=length(unique(A[[i]][[j]][[k]]$NetworkID)),
					ncol=length(unique(A[[i]][[j]][[k]]$NetworkID)),
					dimnames=list(
						unique(A[[i]][[j]][[k]]$NetworkID),
						unique(A[[i]][[j]][[k]]$NetworkID)
					),
					1
				)
				print(unique(A[[i]][[j]][[k]]$NetworkID))
				# get B[[i]][[j]] indices for Z information
				indxB <- outer(
					rownames(B[[i]][[j]]), 
					colnames(B[[i]][[j]]), 
					FUN=paste
				) %in% outer(
					rownames(Z), 
					colnames(Z), 
					FUN=paste
				)
				B1 <- zeros
				# fill B1 with values from Z at indxB locations
				B1[indxB] <- Z
# need to replace diagonal with zeros? or it should be equal to pi_i, how would I do that?

# ARE B and B ordered the same?
				rowcolequal <- paste(
					all(rownames(B1)==rownames(B[[i]][[j]])),
					all(colnames(B1)==colnames(B[[i]][[j]]))
				)
				
				B[[i]][[j]] <- B1 + B[[i]][[j]]
				# SUMMARIZE M INFORMATION
				if (dim(data_networks)[1] > 0) {
					temp <- data_networks %>%
						group_by(NetworkID) %>%
						dplyr::summarise(m = m[1]) %>%
						dplyr::summarise(
							MEAN = mean(m),
							MAX = max(m),
							MIN = min(m),
							MEDIAN = median(m)
						)
					A[[i]][[j]][[k]]$mean_m_unique_neigh 		<- temp$MEAN
					A[[i]][[j]][[k]]$max_m_unique_neigh 		<- temp$MAX
					A[[i]][[j]][[k]]$min_m_unique_neigh 		<- temp$MIN
					A[[i]][[j]][[k]]$median_m_unique_neigh 		<- temp$MEDIAN
					A[[i]][[j]][[k]] <- data_networks %>%
						dplyr::summarise(
							MEAN = mean(m),
							MAX = max(m),
							MIN = min(m),
							MEDIAN = median(m)
						)
					A[[i]][[j]][[k]]$rowcolequal <- rowcolequal
				} else {
					A[[i]][[j]][[k]]$mean_m 	<- 0
					A[[i]][[j]][[k]]$max_m 		<- 0
					A[[i]][[j]][[k]]$min_m 		<- 0
					A[[i]][[j]][[k]]$median_m 	<- 0
					A[[i]][[j]][[k]]$rowcolequal <- NA
					
				}
				A[[i]][[j]][[k]]$sample 			<- k
				A[[i]][[j]][[k]]$seed 				<- temp_seed
				A[[i]][[j]][[k]]$SamplingDesign 	<- SamplingDesign
				A[[i]][[j]][[k]]$simulations 		<- simulations
				A[[i]][[j]][[k]]$pop 				<- P$pop[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	<- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			XY <- list(X, B[[i]][[j]])
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
	newlist %<>% unlist(recursive=F)
	k=1
	newlist2 <- list()
	for (i in 1:n.pop) {
		for (j in 1:nsample.length) {
			newlist2[[k]] <- C[[i]][[j]][grep("dat", names(C[[i]][[j]]))]
			k <- k+1
		}
	}
	newlist2 %<>% unlist(recursive=F) %>% do.call(rbind.data.frame, .)
	print(Sys.time() - TIME)
	return(list(newlist2, newlist))
}