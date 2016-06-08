#' Calculate joint inclusion probability of unit $j$ and $h$
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Vector of values giving the number of units satisfying the ACS criterion in network $i$.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples 
#' # Thompson sampling book, ch. 24 exercises, p. 307, number 2
#' N=1000
#' n1=100
#' m=c(2,3,rep(1,98))
#' pi_jh(N, n1, m) %>% .[1,2]
#' @export

pi_jh <- function(N, n1, m) {
	N.n1 = choose(N, n1)
	N_m.n1 = sapply(
		m, 
		function(m) choose(N - m, n1)
	) # vector of binom(N-mj, n1)
	pi_ = as.data.frame(matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	)) # store pi_jh's
	N_m_m.n1 = as.data.frame(matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	)) # store binom(N-mj-mh, n1)
	for (j in 1 : length(m)) {
	  N_m_m.n1[j, ] = choose((N - m[j] - m), n1)
	  for (h in 1 : length(m)) {
	    # create pi_jh's
	    pi_[j, h] <- 1 - (N_m.n1[j] + N_m.n1[h] - N_m_m.n1[j, h]) / N.n1
	  }
	}
	return(pi_)
}