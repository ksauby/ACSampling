#' Calculate the inclusion probabilities of units in a simple random sample without replacement. 
#' @template N_n1_m_vec
#' @return vector of inclusion probabilities

#' @examples 
#' # example from section 4, p. 1055 (first column) in Thompson (1990)
#' N = 5
#' n1 = 2
#' y = c(1,10,1000)
#' m_vec = c(1,2,2)
#' pi_i(N, n1, m)


#' # EXAMPLE 4:
#' # Ch. 24, Exercise #2, p. 307, from Thompson (2002)
#' N=1000
#' n1=100
#' m_vec=c(2,3,rep(1,98))
#' head(pi_i(N, n1, m_vec))
#' @export

pi_i <- function(N, n1, m_vec) {
  sapply(m_vec, function(m_vec) 
    1 - exp(
		sum(log({N - m_vec - n1 + 1} : {N - m_vec})) - sum(log({N - n1 + 1} : N))
		)
  )
}

#' Calculate the inclusion probabilities of units in a simple random sample with replacement. 
#' @template N_n1_m_vec
#' @return vector of inclusion probabilities

pi_i_replace <- function(N, n1, m_vec) {
     sapply(m_vec, function(m_vec) 1 - (1 - m_vec/N)^n1)
}