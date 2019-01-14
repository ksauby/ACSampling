#' Create a Simple Random Sample Without Replacement.
#' 
#' @param population grid of population to be sampled.
#' @param seed vector of numbers to feed to \code{set.seed()} so that the sampling is reproducible. Defaults to NA so that it is not necessary to specific a random number seed.
#' @param n1 initial sample size (sampled according to simple random sampling without replacement).
#' @param wreplacement Should sampling be done with replacement? Defaults to FALSE.
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

createSRS <- function(population, n1, seed=NA, wreplacement=F) {
	if (!is.na(seed)) {set.seed(seed)}
	sample <- population[sample(
		x 		= 1:dim(population)[1], 
		size 	= n1, 
		replace = wreplacement
	), ]
	# S = merge(population, sample, all.y=TRUE) 	
	if (wreplacement==F) {
		sample$Sampling <- "SRSWOR"
	} else {
		sample$Sampling <- "SRSWR"
	}
	return(sample)		
}
