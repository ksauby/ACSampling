test_that("R_hat, Horvitz-Thompson Ratio Estimator, without replacement", {
	N=100
	n1=13
	m=c(5,5,5,5,5,3,3,3,1,1,1,1,1)
	m_threshold=4
	pi_ij_RACS_mthresh_6 	<- pi_ij_RACS(N,n1,m,m_threshold=6)
	pi_ij_RACS_no_mthresh 	<- pi_ij(N,n1,m)
	pi_ij_RACS_mthresh_4 	<- pi_ij_RACS(N,n1,m,m_threshold)

	# ARE PI_IJ FORMULAS EQUIVALENT WHEN ALL M ARE BELOW M_THRESHOLD
	
	# MANUALLY CALCULATE AND CHECK VALUES IN MATRIX
	# 3 is not over m_threshold
	# 3 is not over m_threshold
	expect_that(
		(vec = as.vector(pi_ij_RACS_mthresh_4[6:8,6:8])),
		 is_equivalent_to(
			rep(
				1 - (
					choose(N-m[6],n1) +
					choose(N-m[6],n1) - 
					choose(N-m[6]-m[6],n1)
				) / choose(N,n1),
				length(vec)
			)
		)	
	)
	# 5 is over threshold
	# 3 is not
	expect_that(
		(vec = c(
			pi_ij_RACS_mthresh_4[1:5,6:8],
			pi_ij_RACS_mthresh_4[6:8,1:5]
		)),
		 is_equivalent_to(
			rep(
				1 - (
					choose(N-m_threshold,n1) +
					choose(N-m[6],n1) - 
					choose(N-m_threshold-m[6],n1)
				) / choose(N,n1),
				length(vec)
			)
		)	
	)
	# 5 is over threshold
	# 1 is not
	expect_that(
		(vec = c(
			pi_ij_RACS_mthresh_4[1:5,9:13],
			pi_ij_RACS_mthresh_4[9:13,1:5]
		)),
		 is_equivalent_to(
			rep(
				1 - (
					choose(N-m_threshold,n1) +
					choose(N-m[9],n1) - 
					choose(N-m_threshold-m[9],n1)
				) / choose(N,n1),
				length(vec)
			)
		)	
	)
	# 3 is not over the threshold
	# 1 is not over the threshold
	expect_that(
		(vec = c(
			pi_ij_RACS_mthresh_4[6:8,9:13],
			pi_ij_RACS_mthresh_4[9:13,6:8]
		)),
		 is_equivalent_to(
			rep(
				1 - (
					choose(N-m[6],n1) +
					choose(N-m[9],n1) - 
					choose(N-m[6]-m[9],n1)
				) / choose(N,n1),
				length(vec)
			)
		)	
	)
	# 1 is not over the threshold
	# 1 is not over the threshold	
	expect_that(
		(vec = as.vector(pi_ij_RACS_mthresh_4[9:13,9:13])),
		 is_equivalent_to(
			rep(
				1 - (
					choose(N-m[9],n1) +
					choose(N-m[9],n1) - 
					choose(N-m[9]-m[9],n1)
				) / choose(N,n1),
				length(vec)
			)
		)	
	)	
}
)