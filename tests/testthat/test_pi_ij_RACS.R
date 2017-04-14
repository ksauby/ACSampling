test_that("R_hat, Horvitz-Thompson Ratio Estimator, without replacement", {
	
		
		
pi_ij_RACS(N=100,n1=10,m=c(5,5,5,5,5,1,1,1,1,1),m_threshold=6)
pi_ij(N=100,n1=10,m=c(5,5,5,5,5,1,1,1,1,1))
pi_ij_RACS(N=100,n1=10,m=c(5,5,5,5,5,1,1,1,1,1),m_threshold=4)

N=100;n1=10;m=c(5,5,5,5,5,1,1,1,1,1);m_threshold=4
1 - (#
					choose(N-m_threshold,n1) + #
					choose(N-m[6],n1) - #
					choose(N-m_threshold-m[6],n1)#
				) / choose(N,n1)
1 - (#
					choose(N-m[1],n1) + #
					choose(N-m[6],n1) - #
					choose(N-m[1]-m[6],n1)#
				) / choose(N,n1)
1 - (#
					2*choose(N-m_threshold,n1) -#
					choose(N-2*m_threshold,n1)#
				) / choose(N,n1)
				
				
				
	expect_that(
				
		equals(
				
			)
			)
			