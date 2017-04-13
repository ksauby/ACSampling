#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;



//[[Rcpp::export]]
Rcpp::NumericMatrix pi_ij_cpp(
	Rcpp::NumericVector m, 
	double N_n1,
	Rcpp::NumericVector N_m_n1,
	Rcpp::NumericMatrix N_m_m_n1
) {
	int ms = m.size();
	Rcpp::NumericMatrix pi_(ms, ms); // store pi_ijs
	for(int i = 0; i < ms; ++i) {
		for(int j = 0; j < ms; ++j) {
		    pi_(i, j) = 1 - (N_m_n1(i) + N_m_n1(j) - N_m_m_n1(i, j)) / N_n1;
		}
	}
	return pi_;
}
	
//[[Rcpp::export]]
Rcpp::NumericMatrix pi_ij_RACS_cpp(
	Rcpp::NumericVector m, 
	double N_n1,
	double m_threshold,
	Rcpp::NumericVector N_m_n1,
	Rcpp::NumericMatrix N_m_m_n1
) {
	nchoosek(int N, int K) {
		decimal result = 1;
		for (int i = 1; i <= K; i++)
		{
		    result *= N - (K - i);
		    result /= i;
		}
		return result;
	}
	
	int ms = m.size();
	int m_threshold = m_threshold;
		
	decimal N_n1 = 1;
	N_n1 = &nchoosek(N, n1);
	
	decimal N_m_threshold_n1 = 1;
	N_n1 = &nchoosek(N - m_threshold, n1);
	
	decimal N_2m_threshold_n1 = 1;
	N_n1 = &nchoosek(N - 2*m_threshold, n1);
		
	
	
	# vector of binom(N-mj, n1)
	N_m_n1 	<- sapply(m, function(m) choose(N - m, n1)) 
	N_m_m_n1 = matrix(
		nrow = length(m), 
		ncol = length(m), 
		NA
	) # store binom(N-mj-mh, n1)
	Rcpp::NumericMatrix N_m_m_n1_(ms, ms); // store pi_ijs
	
	for (j in 1 : length(m)) {
	  N_m_m_n1[j, ] = choose((N - m[j] - m), n1)
	}	
	
	
	Rcpp::NumericMatrix pi_(ms, ms); // store pi_ijs
	for(int i = 0; i < ms; ++i) {
		for(int j = 0; j < ms; ++j) {
			if ( m(i) <= m_threshold & m(j) <= m_threshold ) {
				pi_(i, j) = 1 - (N_m_n1(i) + N_m_n1(j) - N_m_m_n1(i, j)) / N_n1;
			}
			if ( m(i) <= m_threshold & m(j) <= m_threshold ) {
				pi_(i, j) = 1 - (N_m_threshold_n1 + N_m_n1(j) - N_m_m_n1(i, j)) / N_n1;
			}
			if ( m(i) <= m_threshold & m(j) <= m_threshold ) {
				pi_(i, j) = 1 - (N_m_n1(i) + N_m_threshold_n1 - N_m_m_n1(i, j)) / N_n1;
			}
			if ( m(i) <= m_threshold & m(j) <= m_threshold ) {
				pi_(i, j) = 1 - (2*N_m_threshold_n1 - N_2m_threshold_n1) / N_n1;
			}
			
					
				}
			}	
			
			
		}
	}
	return pi_;
}
	




//[[Rcpp::export]]
Rcpp::NumericMatrix var_y_HT_cpp(
	Rcpp::NumericVector m, 
	Rcpp::NumericVector pi_i_values, 
	Rcpp::NumericMatrix pi_ij_values, 
	Rcpp::NumericVector y
) {
	int n = m.size();
	Rcpp::NumericMatrix V(n, n);	
	for(int i = 0; i < n; ++i) {
		for(int j = 0; j < n; ++j) {
			V(i, j) = 	y(i) * y(j) * 
				(pi_ij_values(i, j) - pi_i_values(i) * pi_i_values(j)) / 
				(pi_i_values(i) * pi_i_values(j) * pi_ij_values(i, j));
		}
	}
	return V;
}
// [[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export]]
Rcpp::NumericMatrix R_hat_cpp(
	Rcpp::NumericVector y_hat, 
	Rcpp::NumericVector pi_i_values,
	Rcpp::NumericMatrix pi_ij_values
) {
	int n = y_hat.size();
	Rcpp::NumericMatrix V(n, n); // store pi_ijs
	for(int i = 0; i < n; ++i) {
		for(int j = 0; j < n; ++j) {
		    V(i, j) = 2 * y_hat(i) * y_hat(j) * 
					(
						1/(pi_i_values(i) * pi_i_values(j)) - 
						1/pi_ij_values(i, j)
					);
		}
	}
	arma::mat V_ = Rcpp::as<arma::mat> (V);
	V_.diag().zeros();
	return Rcpp::wrap( V_ );
}