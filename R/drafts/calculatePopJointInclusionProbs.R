# Create list of joint inclusion probabilities for all unique network sizes for each realization of each cactus patch density
#' @ description For each realization of each cactus patch density (each unique \code{n.network}), create a matrix with all possible joint inclusion probabilities. The number of rows and columns of each matrix are determined by the number of unique network sizes per realization of a cactus patch density. The results are stored in a list.
#' @returns A list of matrices including the joint inclusion probabilities.
#' @param patch_data
#' @param nsamples a vector of sample sizes
#' @param N population size
#' @export

calculatePopJointInclusionProbs <- function(patch_data, nsamples, N) {
	patch_data %<>% arrange(n.networks, realization)
	A <- vector("list", length(n.networks))
	for (i in 1:length(unique(patch_data$n.networks))) {
		for (j in 1:length(unique(patch_data$realization))) {
			P <- patch_data %>% 
				filter(
					n.networks==unique(patch_data$n.networks)[i],
					realization==unique(patch_data$realization)[j]
				)
				for (k in 1:length(nsamples)) {
					A[[i]][[j]][[k]] <- pi_jh(
						N = N,
						n1 = nsamples[k],
						m = unique(P$m)	
					)
					dimnames(A[[i]][[j]][[k]]) <- 
						list(unique(P$m), unique(P$m))	
			}
		}
	}
	return(A)
}
