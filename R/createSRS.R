#' Create a Simple Random Sample Without Replacement.
#' 
#' @param popdata grid of population to be sampled.
#' @param seed vector of numbers to feed to \code{set.seed()} so that the sampling is reproducible. Defaults to NA so that it is not necessary to specific a random number seed.
#' @param n1 initial sample size (sampled according to simple random sampling without replacement).
#' @param replace Should sampling be done with replacement? Defaults to FALSE.
#' @return A restricted adaptive cluster sample.
#' @examples
#' # example
#' # create the population
#' # create the patch
#' # then sample
#' 
#' data(Thompson1990Figure1Population)
#' data(Thompson1990Figure1Sample)
#' Z = createSRS(Thompson1990Figure1Population, seed=2, n1=10)
#' @export

createSRS <- function(popdata, n1, seed=NA, replace=F) {
	if (!is.na(seed)) {set.seed(seed)}
	sample <- popdata[sample(
		x 		= 1:dim(popdata)[1], 
		size 	= n1, 
		replace = replace
	), ]
	if (replace==F) {
		sample$Sampling <- "SRSWOR"
	} else {
		sample$Sampling <- "SRSWR"
	}
	return(sample)		
}
