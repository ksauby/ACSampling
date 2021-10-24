#' Calculate joint inclusion probability of unit $j$ and $h$
#' @template N
#' @template n1
#' @template m_vec
#' @references
#' @template Thompson1990
#' @examples 
#' # Thompson sampling book, ch. 24 exercises, p. 307, number 2
#' library(magrittr)
#' N=1000
#' n1=100
#' m=c(2,3,rep(1,98))
#' pi_ij(N, n1, m) %>% .[1,2]
#' @export

pi_ij <- function(N, n1, m_vec) {
	N_n1 <- choose(N, n1)
	# vector of binom(N-mj, n1)
	N_m_n1 <- sapply(m_vec, function(m_vec) choose(N - m_vec, n1)) 
	N_m_m_n1 = matrix(
		nrow = length(m_vec), 
		ncol = length(m_vec), 
		NA
	) # store binom(N-mj-mh, n1)
	for (j in 1 : length(m_vec)) {
          N_m_m_n1[j, ] = choose((N - m_vec[j] - m_vec), n1)
	}	
	pi_ij_cpp(m_vec, N_n1, N_m_n1, N_m_m_n1)
}

#' Calculate joint inclusion probability of unit $j$ and $h$, using the RACS correction
#' @template N
#' @template n1
#' @template m_vec
#' @template m_threshold
#' @template SaubyCitation
#' @export

pi_ij_RACS <- function(N, n1, m_vec, m_threshold) {
	N_n1 <- choose(N, n1)
	N_m_threshold_n1 <- choose(N - m_threshold, n1)
	N_2m_threshold_n1 <- choose(N - 2*m_threshold, n1)
	# vector of binom(N-mj, n1)
	N_m_n1 <- sapply(m_vec, function(m_vec) choose(N - m_vec, n1)) 
	N_m_m_n1 = matrix(
		nrow = length(m_vec), 
		ncol = length(m_vec), 
		NA
	) # store binom(N-mj-mh, n1)
	N_m_threshold_m_n1 = NA
	for (j in 1 : length(m_vec)) {
	  N_m_m_n1[j, ] = choose((N - m_vec[j] - m_vec), n1)
	}
	for (j in 1 : length(m_vec)) {
	  N_m_threshold_m_n1[j] = choose((N - m_threshold - m_vec[j]), n1)
	}
	pi_ij_RACS_cpp(
	     m_vec, 
		N_n1, 
		N_m_threshold_n1, 
		N_2m_threshold_n1,
		m_threshold,
		N_m_n1,
		N_m_threshold_m_n1,
		N_m_m_n1
	)
}

#' Calculate joint inclusion probability of unit $j$ and $h$ when sampling occurs with replacement
#' @template N
#' @template n1
#' @template m_vec
#' @export

pi_ij_replace <- function(N, n1, m_vec) {
     pi_ij = matrix(
          nrow = length(m_vec), 
          ncol = length(m_vec), 
          NA
     )
     for (i in 1 : length(m_vec)) {
          for (j in 1 : length(m_vec)) {
               pi_ij[i, j] <- 
                    1 - (
                         (1 - m_vec[i]/N)^n1 + 
                              (1 - m_vec[j]/N)^n1 -
                              (1 - (m_vec[i] + m_vec[j])/N)^n1
                    )
          }
     }
     return(pi_ij)
}