#' Create the population displayed in Figure 1 from Thompson (1990)
#' 
#' @return The population displayed in Figure 1 from Thompson (1990).
#' @examples createThompson1990Figure1Sample()
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @export

createThompson1990Figure1Sample <- function() {
	sample = rbind(
			c(6,19),
			c(18,18),
			c(4,17),
			c(4,16),
			c(19,12),
			c(12,11),
			c(11,7),
			c(5,6),
			c(4,2),
			c(16,1)
		) %>%
		as.data.frame
	names(sample) <- c("x", "y")
	return(sample)
}



# INITIAL STAGE OF SAMPLING
	