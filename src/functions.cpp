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
Rcpp::NumericMatrix var_t_HT_cpp(
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