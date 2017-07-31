test_that("Hajek, Hajek Variance Approximation", {
	# p. 138, from Tille, Y (2006) Sampling Algorithms. Springer.
	N=6
	n=3
	pi_i_values = c(0.07, 0.17, 0.41, 0.61, 0.83, 0.91)

	
	
	expect_that(
		Hajek(pi_i = pi_i_values, n = N),
		equals(c(0.07812, 0.16932, 0.29028, 0.28548, 0.16932, 0.09828))
	)
})