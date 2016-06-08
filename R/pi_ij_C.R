#' Calculate joint inclusion probability of unit $j$ and $h$
#' @param N Population size.
#' @param n1 Initial sample size.
#' @param m Vector of values giving the number of units satisfying the ACS criterion in network $i$.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples 
#' # Thompson sampling book, ch. 24 exercises, p. 307, number 2
#' library(magrittr)
#' N=1000
#' n1=100
#' m=c(2,3,rep(1,98))
#' pi_ij(N, n1, m) %>% .[1,2]
#' @export

pi_ij <- function(N, n1, m) {
	N_n1 	<- choose(N, n1)
	# vector of binom(N-mj, n1)
	N_m_n1 	<- sapply(m, function(m) choose(N - m, n1)) 
	N_m_m_n1 = matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	) # store binom(N-mj-mh, n1)
	for (j in 1 : length(m)) {
	  N_m_m_n1[j, ] = choose((N - m[j] - m), n1)
	}	
	pi_ij_cpp(m, N_n1, N_m_n1, N_m_m_n1)
}