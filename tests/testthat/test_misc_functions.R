#### TESTING
test_that("NA_is_Zero_Function", {
  
     expect_equal(
          NA_is_Zero_Function(NA),
          0
     )
})

test_that("Sum", {

	expect_equal(
	     Sum(c(1,2,NA)),
		3
	)
})
test_that("Mean", {
     expect_equal(
          Mean(c(1,2,NA)),
		1.5
	)
})
test_that("Population Coefficient of Variation", {
     x =  c(1,2,1,3,1,4,1,5,NA)
     mean_x = Mean(x)
     var_x = popVar(x)
     expect_equal(
          sqrt(var_x)/mean_x,
          popCV(x)
     )
})


test_that("population variance, popVar", {
     
     x = c(16,11,9,8,1)
     mu = 9
     var_x <- sum(
          (16-mu)^2,
          (11-mu)^2,
          (9-mu)^2,
          (8-mu)^2,
          (1-mu)^2
     )/length(x)
     
     expect_equal(
          var_x,
          popVar(x)
     )
     
     x <- mu <- var_x <- NULL
     
     x = c(1:10)
     mu=5.5
     var_x <- sum(
          (1-mu)^2,
          (2-mu)^2,
          (3-mu)^2,
          (4-mu)^2,
          (5-mu)^2,
          (6-mu)^2,
          (7-mu)^2,
          (8-mu)^2,
          (9-mu)^2,
          (10-mu)^2
     )/length(x)
     
     expect_equal(
          var_x,
          popVar(x)
     )
})

