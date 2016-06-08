pi_jh_C <- function(N, n1, m) {
	N_n1 	<- choose(N, n1)
	# vector of binom(N-mj, n1)
	N_m_n1 	<- sapply(m, function(m) choose(N - m, n1)) 
	pi_jk_cpp(m, N, n1, N_n1, N_m_n1)
}	
	microbenchmark(pi_jh_C(N, n1, m), pi_jh(N, n1, m))
	'
	
	
cppFunction('NumericMatrix pi_jk_cpp(
	NumericVector m, 
	int N, 
	int n1, 
	int N_n1,
	NumericVector N_m_n1
) {
	int ms = m.size();
	NumericMatrix pi_(ms, ms); // store pi_jhs
	NumericMatrix N_m_m_n1(ms, ms); // store binom(N-mj-mh, n1)
	for(int j = 0; j < ms; ++j) {
		for(int k = 0; k < ms; ++k) {
		
			int n = N - m(j) - m(k);
			int result = n;
			int K = n1;
		
		    if (K > n) return 0;
		    if (K * 2 > n) K = n-K;
		    if (K == 0) return 1;
				
		    for( int l = 2; l <= K; ++l ) {
		        result *= (n-l+1);
		        result /= l;
		    }
		
			N_m_m_n1(j, k) = result;
			// create pi_jhs
		    pi_(j, k) = 1 - (N_m_n1(j) + N_m_n1(k) - N_m_m_n1(j, k)) / N_n1;
		}
	}
	return pi_;
}')
	
	
cppFunction('int nChoosek( int n, int k )
{
    if (k > n) return 0;
    if (k * 2 > n) k = n-k;
    if (k == 0) return 1;

    int result = n;
    for( int i = 2; i <= k; ++i ) {
        result *= (n-i+1);
        result /= i;
    }
    return result;
}
')