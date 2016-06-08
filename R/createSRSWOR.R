#' Create an Adaptive Cluster Sample.
#' 
#' @param population grid of population to be sampled.
#' @param seed vector of numbers to feed to \code{set.seed()} so that the sampling is reproducible.
#' @param n1 initial sample size (sampled according to simple random sampling without replacement).
#' @return A restricted adaptive cluster sample.
#' @examples
#' # example
#' # create the population
#' # create the patch
#' # then sample
#' 
#' data(Thompson1990Figure1Population)
#' data(Thompson1990Figure1Sample)
#' Z = createSRSWOR(Thompson1990Figure1Population, seed=2, n1=10)
#' @export

createSRSWOR <- function(population, seed, n1) {
	set.seed(seed)
	sample = population[sample(x=1:dim(population)[1], size=n1, replace=F), ]
	# S = merge(population, sample, all.y=TRUE) 	
	sample$Sampling <- "SRSWOR"
	return(sample)		
}
