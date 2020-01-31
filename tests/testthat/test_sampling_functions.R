#### TESTING

# createSRS
test_that("1. createSRS, Does the function work when setting the seed for random sampling?", {
	data(Thompson1990Fig1Pop)
	Z <- createSRS(Thompson1990Fig1Pop, 10, seed=26)
	expect_equal(
		dim(Z[which(Z$y_value==2), ])[1],
		1
	)
})
test_that("2. createSRS, Does the function work without setting the seed for random sampling?", {
	Z <- createSRS(Thompson1990Fig1Pop, 10)
	expect_equal(
		dim(Z)[1],
		10
	)
})
test_that("3. createSRS, Does the function work for without replacement sampling?", {
	Z <- createSRS(Thompson1990Fig1Pop, 10)
	expect_equal(
		dim(Z[which(Z$Sampling=="SRSWOR"), ])[1],
		10
	)
})
test_that("4. createSRS, Does the function work for with replacement sampling?", {
	Z <- createSRS(Thompson1990Fig1Pop, 10, replace=T)
	expect_equal(
		dim(Z[which(Z$Sampling=="SRSWR"), ])[1],
		10
	)
})
# createACS
test_that("5. createACS, Does the function work when providing the seed and without providing the initial sample? Example 1: no adaptive cluster sampling takes place.", {
	data(Thompson1990Fig1Pop)
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value", seed=2)
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		0
	)
})
test_that("6. createACS, Does the function work when providing the seed and without providing the initial sample? Example 2: adaptive cluster sampling takes place", {
	data(Thompson1990Fig1Pop)
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		11
	)
})
test_that("7. createACS, Does the function work when providing the initial sample?", {
	S <- createSRS(Thompson1990Fig1Pop, 10, seed=2)
	S[5, c("x", "y")] <- c(10,6)
	init <- S[, c("x", "y")]
	Z <- createACS(
		popdata=Thompson1990Fig1Pop, 
		n1=10, 
		yvar="y_value", 
		initsample=init
	) 
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		11
	)
})
test_that("8. createACS, Are y-values of edge units equal to 0?", {
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		unique(Z[which(Z$Sampling == "Edge"), ]$y_value),
		0
	)
})
test_that("9. createACS, Are m-values of edge units 0?", {
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		unique(Z[which(Z$Sampling == "Edge"), ]$m),
		0
	)
})
test_that("10. createACS, Does the function work when no seed or initial sample is provided?", {
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value")
	expect_gte(
		dim(Z)[1],
		10
	)
})
test_that("11. createACS, Are there duplicates units in the sample?", {
	Z <- createACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		dim(Z[duplicated(Z), ])[1],
		0
	)
})
# createRACS
test_that("12. createRACS, Does the function work when providing the seed and without providing the initial sample? Example 1: no adaptive cluster sampling takes place.", {
	data(Thompson1990Fig1Pop)
	Z <- createRACS(popdata=Thompson1990Fig1Pop, n1=10, yvar="y_value", seed=5)
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		0
	)
})
test_that("13. createRACS, Does the function work when providing the seed and without providing the initial sample? Example 2: adaptive cluster sampling takes place", {
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		4
	)
})
test_that("14. createRACS, Does the function work when providing the initial sample?", {
	S <- createSRS(Thompson1990Fig1Pop, 10, seed=2)
	S[5, c("x", "y")] <- c(10,6)
	init <- S[, c("x", "y")]
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value", 
		initsample=init) 
	expect_equal(
		dim(Z[which(Z$y_value > 0), ])[1],
		9
	)
})
test_that("15. createRACS, Are y-values of edge units equal to 0?", {
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		unique(Z[which(Z$Sampling == "Edge"), ]$y_value),
		0
	)
})
test_that("16. createRACS, Are m-values of edge units 0?", {
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		unique(Z[which(Z$Sampling == "Edge"), ]$m),
		0
	)
})
test_that("17. createRACS, Does the function work when no seed or initial sample is provided?", {
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value")
	expect_gte(
		dim(Z)[1],
		10
	)
})

test_that("18. createRACS, Are there duplicates units in the sample?", {
	Z <- createRACS(Thompson1990Fig1Pop, 10, "y_value", seed=26)
	expect_equal(
		dim(Z[duplicated(Z), ])[1],
		0
	)
})