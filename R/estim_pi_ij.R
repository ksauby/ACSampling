#' Calculate Joint Inclusion Probabilities Using Simulations

#' @template popdata
#' @param sims Number of simulations per population.
#' @template n1_vec
#' @template SamplingDesign_no_ACS
#' @template yvar
#' @template f_max
#' @template popvar_default_null

#' @description Calculate inclusion probabilities for each unit in a population using simulations.

#' @return A list of n1_vec and dat, BUT I DONT REMEMBER WHAT THOSE ARE.

#' @importFrom foreach %:% 
#' @importFrom data.table as.data.table setnames
#' @export

estim_pi_ij <- function(
	popdata, 
	sims, 
	n1_vec, 
	SamplingDesign="ACS",
	y_variable,
	f_max = NULL,
	popvar=NULL
) 
{
	pop <- i <- j <- Sampling <- . <- NetworkID <- NULL
	TIME 			<- Sys.time()
	popdata 		%<>% arrange(!! sym(popvar))
	if (popvar != "pop") {
		colnames(popdata)[which(names(popdata) == popvar)] <- "pop"
	}
	n.pop 			<- length(unique(popdata$pop))
	nsample.length 	<- length(n1_vec)
	A 				<- vector("list", n.pop)
	B 				<- vector("list", n.pop)
	C = foreach (
		i = 1:n.pop, # for each species density
		.inorder = FALSE, 
		.packages = c(
		     "magrittr", 
		     "foreach", 
		     "plyr", 
		     "dplyr", 
		     "data.table",
		 	"ACSampling", 
		 	"intergraph", 
		 	"network", 
		 	"igraph", 
		 	"stringr"
		 ), 
		.combine = "list",
		.multicombine = TRUE
		) %:%
	 	foreach (
			j = 1:nsample.length, # for each sampling effort
			.multicombine = TRUE,
			.inorder = FALSE
		) %dopar% {
			P <- popdata %>% filter(pop==unique(popdata$pop)[i])
			N <- dim(P)[1]
			n1 <- n1_vec[j]
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
			r <- (i - 1) * j + j
			seeds <- runif(sims)
		   for (k in 1:sims) {
				temp_seed <- seeds[k]*100000
				if (SamplingDesign=="ACS") {
					alldata <- createACS(
						popdata = P, 
						seed = temp_seed, 
						n1 = n1, 
						yvar = y_variable
					) %>% 
						as.data.table()
				} else {
					alldata <- createRACS(
						popdata = P, 
						seed = temp_seed, 
						n1 = n1, 
						yvar = y_variable,
						f_max = f_max
					) %>% 
						as.data.table()
				}
				A[[i]][[j]][[k]] <- alldata %>% 
					filter(Sampling!="Edge") %>%
					dplyr::select(
					     pop, 
					     .data$x, 
					     .data$y, 
					     NetworkID, 
					     Sampling
					) %>%
					arrange(NetworkID)
				     # save for m summary data
				     data_networks <- alldata %>%
					     filter(.data$m!=0)
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
						dplyr::summarise(m = .data$m[1]) %>%
						dplyr::summarise(
							MEAN = mean(.data$m),
							MAX = max(.data$m),
							MIN = min(.data$m),
							MEDIAN = median(.data$m)
						)
					A[[i]][[j]][[k]]$mean_m_unique_neigh <- temp$MEAN
					A[[i]][[j]][[k]]$max_m_unique_neigh <- temp$MAX
					A[[i]][[j]][[k]]$min_m_unique_neigh <- temp$MIN
					A[[i]][[j]][[k]]$median_m_unique_neigh <- temp$MEDIAN
					A[[i]][[j]][[k]] <- data_networks %>%
						dplyr::summarise(
							MEAN = mean(.data$m),
							MAX = max(.data$m),
							MIN = min(.data$m),
							MEDIAN = median(.data$m)
						)
					A[[i]][[j]][[k]]$rowcolequal <- rowcolequal
				} else {
					A[[i]][[j]][[k]]$mean_m <- 0
					A[[i]][[j]][[k]]$max_m <- 0
					A[[i]][[j]][[k]]$min_m <- 0
					A[[i]][[j]][[k]]$median_m <- 0
					A[[i]][[j]][[k]]$rowcolequal <- NA
				}
				A[[i]][[j]][[k]]$sample <- k
				A[[i]][[j]][[k]]$seed <- temp_seed
				A[[i]][[j]][[k]]$SamplingDesign <- SamplingDesign
				A[[i]][[j]][[k]]$sims <- sims
				A[[i]][[j]][[k]]$pop <- P$pop[1]
				A[[i]][[j]][[k]]$N.SRSWOR.plots <- n1
			}
			X <- do.call(rbind.data.frame, A[[i]][[j]])
			XY <- list(X, B[[i]][[j]])
			names(XY) <- 
			c(
				"dat",
				paste(
					"n1_vec",
					n1_vec[j],
					"pop",
					unique(popdata$pop)[i],
					sep="_"
				)
			)
			XY
	}
	k=1
	newlist <- list()
	for (i in 1:n.pop) {
		for (j in 1:nsample.length) {
			newlist[[k]] <- C[[i]][[j]][grep("n1_vec", names(C[[i]][[j]]))]
			k <- k + 1
		}
	}
	newlist %<>% unlist(recursive = FALSE)
	k=1
	newlist2 <- list()
	for (i in 1:n.pop) {
		for (j in 1:nsample.length) {
			newlist2[[k]] <- C[[i]][[j]][grep("dat", names(C[[i]][[j]]))]
			k <- k + 1
		}
	}
	newlist2 %<>% 
	     unlist(recursive = FALSE) %>% 
	     do.call(rbind.data.frame, .)
	print(Sys.time() - TIME)
	return(list(newlist2, newlist))
}