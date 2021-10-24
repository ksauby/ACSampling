#### TESTING
test_that("NA_is_Zero_Function", {
  
     expect_equal(
          ACSampling:::NA_is_Zero_Function(NA),
          0
     )
})

test_that("Sum", {

	expect_equal(
	     ACSampling:::Sum(c(1,2,NA)),
		3
	)
})
test_that("Mean", {
     expect_equal(
          ACSampling:::Mean(c(1,2,NA)),
		1.5
	)
})
