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
	B 						<- vector("list", n.patches)
	C = foreach (
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
					dplyr::select(n.networks, realization, x, y)
				cactus_networks <- alldata %>%
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
				
				
				B1 <- A1
				indxA <- outer(rAB, cAB, FUN=paste) %in% outer(rownames(pop.matrix), colnames(pop.matrix), FUN=paste) 
				A1[indxA] <- pop.matrix
				
				
				
				
				
				
				
				m3 <- pop.matrix
				mcol <- match(
					colnames(pop.matrix),
					colnames(B[[i]][[j]][[k]])
				)
				
				
				new <- m3[,mcol]<-m3[,mcol]+B[[i]][[j]][[k]]
				
				
		B[[i]][[j]][[1]][,colnames(m3)]		
				
				
				
				
				
				
		m1<-matrix(1,3,5)
		colnames(m1)<-LETTERS[1:5]
		m2<-matrix(1:9,3,3)
		colnames(m2)<-c("D","A","C")
		m1
		m2
		m3<-m1
		mcol<-match(colnames(m2),colnames(m1))
		m3[,mcol]<-m3[,mcol]+m2
		m3
				
				
				
				
				
				
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
				A[[i]][[j]][[k]]$sample 			<- k
				A[[i]][[j]][[k]]$seed 				<- temp_seed
				A[[i]][[j]][[k]]$SamplingDesign 	<- SamplingDesign
				A[[i]][[j]][[k]]$simulations 		<- simulations
				A[[i]][[j]][[k]]$realization 		<- P$realization[1]
				A[[i]][[j]][[k]]$n.networks 		<- P$n.networks[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots 	<- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			X$coords <- with(X, paste(x,y,sep="_"))
			X <- P %>% 
				dplyr::select(coords, NetworkID) %>%
				merge(X, by="coords")
			B[[i]][[j]][[k]]$joint.inclusion <- matrix(
				nrow=length(unique(X$NetworkID)),
				ncol=length(unique(X$NetworkID)),
				dimnames=list(unique(X$NetworkID),unique(X$NetworkID))
			)
			
			
			joint.inclusions <- matrix(
				nrow=max(max(P$NetworkID)),
				ncol=max(max(P$NetworkID))
			)
			X %>% group_by(sample,NetworkID) %>%
			summarise(n())
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
	C$simulation_date 	= format(Sys.time(), "%m-%d-%y")
	C$f_max 		= f_max
	print(Sys.time() - TIME)
	return(C)
}