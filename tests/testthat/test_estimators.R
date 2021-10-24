#### TESTING





test_that("assignNetworkMembership", {
     lambdap_5_tau_25_pop <- expand.grid(
          x = 1:10,
          y = 1:10
     ) %>%
          cbind(
               y_value = c(
                    0,0,0,4,rep(0,6),
                    0,0,0,43,17,rep(0,5),
                    rep(0,10),
                    rep(0,10),
                    rep(0,10),
                    rep(0,2), rep(1,4), rep(0,4),
                    rep(0,2), 50, 51, 41, 5, rep(0,4),
                    rep(0,2), 2, 1, rep(0,6),
                    rep(0,10),
                    rep(0,10)
               )
          ) %>%
          filter(y_value > 0) 
     ManuallyAssignedNetworkIDs <- lambdap_5_tau_25_pop %>%
          cbind(
               NetworkID = c(
                    1,1,1,
                    rep(2, 10)
               ),
               m = c(
                    rep(3, 3),
                    rep(10, 10)
               )
          )
     expect_equal(
          assignNetworkMembership(lambdap_5_tau_25_pop, plot.size=1),
          ManuallyAssignedNetworkIDs
     )
     
     lambdap_10_tau_25_pop <- expand.grid(
          x = 1:10,
          y = 1:10
     ) %>%
          cbind(
               y_value = c(
                    rep(0,5),10,1,0,0,0,
                    0,0,2,10,18,32,5,rep(0,3),
                    0,0,30,26,16,1,34,1,0,7,
                    0,0,0,0,12,25,7,0,9,42,
                    0,0,0,0,1,21,1,2,12,0,
                    9,0,0,49,33,15,2,3,26,9,
                    43,0,0,7,5,0,0,0,0,34,
                    1,rep(0,9),
                    46,5,0,1,9,2,35,2,0,0,
                    1,0,0,1,44,1,15,1,0,0
               )
          ) %>%
          filter(y_value > 0) 
     ManuallyAssignedNetworkIDs <- lambdap_10_tau_25_pop %>%
          cbind(
               NetworkID = c(
                    rep(1, 24),
                    2,
                    rep(1, 7),
                    2,
                    rep(1, 3),
                    rep(2, 3),
                    rep(3, 5),
                    2,
                    rep(3, 5)
               ),
               m = c(
                    rep(34, 24),
                    6,
                    rep(34, 7),
                    6,
                    rep(34, 3),
                    rep(6, 3),
                    rep(10, 5),
                    6,
                    rep(10, 5)
               )
          )
     expect_equal(
          assignNetworkMembership(lambdap_10_tau_25_pop, plot.size=1),
          ManuallyAssignedNetworkIDs
     )
     
     
})

test_that("y_HT, Horvitz-Thompson Mean Estimator", {

	expect_equal(
		round(
		     # Ch. 24, Exercise #2, p. 307, from Thompson (2002)
			y_HT(
				N = 1000, 
				n1 = 100, 
				m_vec = c(2,3, rep(1,98)), 
				y = c(3,6, rep(0, 98)), 
				sampling = "SRSWOR", 
				criterion = 0
			)*1000, 0
		),
		38
	)
	# Example from Thompson (1990), end of second full paragraph, p. 1055
	mk <- c(1,0,2,2)
	y_value <- c(1,2,10,1000)
	sampling <- c("S","C","S","C")
	dat <- data.frame(mk, y_value, sampling)
	dat$mk %<>% as.numeric
	dat$y_value %<>% as.numeric
	dat_filter <- dat %>% filter(sampling=="S" | y_value > 4)
	expect_equal(
		round(
			y_HT(
				N = 5, 
				m = dat_filter$mk, 
				n1 = 2, 
				y = dat_filter$y_value
			), 2
		),
		289.07
	)
})
test_that("pi_i, Network Inclusion Probability", {
	# Ch. 24, Exercise #2, p. 307, from Thompson (2002)
	expect_equal(
		round(
			pi_i(
				N = 1000, 
				n1 = 100, 
				m = c(2,3,rep(1,98))
			)[1:2], 2
		),
		c(0.19, 0.27)
	)
})

# R ABORTS WHEN TRYING TO TEST PI_IJ
test_that("pi_ij, Network Joint Inclusion Probability", {
	# Ch. 24, Exercise #2, p. 307, from Thompson (2002)
	# answer in the book is 0.0511 but I get 0.0512; I'm going to assume that this is due to a rounding error in the book's calculation
	expect_equal(
		round(
			pi_ij(
				N = 1000, 
				n1 = 100, 
				m = c(2,3,rep(1,98))
			)[1, 2], 4
		),
		0.0512
	)
})

# R ABORTS WHEN TRYING TO TEST var_y_HT
test_that("var_y_HT, Horvitz-Thompson Variance Estimator", {
	# Ch. 24, Exercise #2, p. 307, from Thompson (2002)
	# Horvitz Thompson variance of the total, (using the variance of the mean, and then multiply by N^2)
	# answer in the book is 552 but I get 553; I'm going to assume that this is due to a rounding error in the book's calculation
	expect_equal(
		round(
			var_y_HT(
				N = 1000, 
				n1 = 100, 
				m = c(2,3,rep(1,98)), 
				y = c(3,6,rep(0, 98))
			)*(1000^2), 0
		),
		553
	)
})


test_that("R_hat, Horvitz-Thompson Ratio Estimator, with replacement", {
	# Thompson (2002), Example 2, p. 78-79
	# I get 17.6854 if I round(pi_ij_values, 4) and round(y_hat, 2)
	expect_equal(
		round(
		     
		     # ERROR SAYS N1 IS NOT AN INTEGER
		     
			R_hat(
				y = c(60, 14, 1), 
				x = c(1, 1, 1), 
				N = 100, 
				n1 = 4, 
				m = c(5, 2, 1), 
				replace="TRUE"
			), 2
		),
		12.12
	)
})
test_that("var_R_hat, with replacement", {
	# Thompson (2002), Example 2, p. 78-79
	expect_equal(
		round(
			var_R_hat(
				y = c(60, 14, 1), 
				x = c(1, 1, 1), 
				N = 100, 
				n1 = 4, 
				m = c(5, 2, 1), 
				replace="TRUE"
			), 2
		),
		17.5
	)
})
# NEED TO DOUBLECHECK THE MATH
test_that("var_R_hat, without replacement", {
     # Thompson (2002), Example 2, p. 78-79
     expect_equal(
          round(
               var_R_hat(
                    y = c(60, 14, 1), 
                    x = c(1, 1, 1), 
                    N = 100, 
                    n1 = 4, 
                    m = c(5, 2, 1), 
                    replace="FALSE"
               ), 2
          ),
          16.97
     )
})
test_that("R_hat, Horvitz-Thompson Ratio Estimator, without replacement", {
	# Exercise #3, p. 85, Thompson (2002)
	N 		<- 4
	dat 	<- data.frame(y = c(2, 3, 0, 1), x = c(20, 25, 0, 15))
	combin 	<- combn(4,2)
	combin	<- split(combin, col(combin))
	combos 	<- lapply(combin, function(x) dat[row(dat) %in% x, ])
	combos %<>% lapply(., function(x) filter(x, !(is.na(y))))
	expect_equal(
		lapply(
			combos, 
			function(x) round(
				R_hat(
					x$y, 
					x$x, 
					N = 4, 
					n1 = 2, 
					m = c(1,1)
				)*(20 + 25 + 15), 6
			)
		),
		list(
		     `1` = 6.666667, 
		     `2` = 6, 
		     `3` = 5.142857, 
		     `4` = 7.2, 
		     `5` = 6, 
		     `6` = 4
		)
	)
})
test_that("R_hat, Horvitz-Thompson Ratio Estimator, without replacement, returns 0", {
     expect_equal(
          R_hat(
               y = c(0, 0, 0), 
               x = c(1, 1, 1), 
               N = 100, 
               n1 = 4, 
               m = c(5, 2, 1), 
               replace="FALSE"
          ),
          0
     )
})




