#### TESTING

# createSRS
test_that("createSRS, Does the function work when setting the seed for random sampling?", {
	data(Thompson1990Figure1Population)
	Z <- createSRS(Thompson1990Figure1Population, 10, seed=26)
	expect_that(
		dim(Z[which(Z$y_value==2), ])[1],
		equals(1)
	)
})
test_that("createSRS, Does the function work without setting the seed for random sampling?", {
	Z <- createSRS(Thompson1990Figure1Population, 10)
	expect_that(
		dim(Z)[1],
		equals(10)
	)
})
test_that("createSRS, Does the function work for without replacement sampling?", {
	Z <- createSRS(Thompson1990Figure1Population, 10)
	expect_that(
		dim(Z[which(Z$Sampling=="SRSWOR"), ])[1],
		equals(10)
	)
})
test_that("createSRS, Does the function work for with replacement sampling?", {
	Z <- createSRS(Thompson1990Figure1Population, 10, wreplacement=T)
	expect_that(
		dim(Z[which(Z$Sampling=="SRSWR"), ])[1],
		equals(10)
	)
})
# createACS
test_that("createACS, Does the function work when providing the seed and without providing the initial sample? Example 1: no adaptive cluster sampling takes place.", {
	data(Thompson1990Figure1Population)
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", seed=2)
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(0)
	)
})
test_that("createACS, Does the function work when providing the seed and without providing the initial sample? Example 2: adaptive cluster sampling takes place", {
	data(Thompson1990Figure1Population)
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(11)
	)
})
test_that("createACS, Does the function work when providing the initial sample?", {
	S <- createSRS(Thompson1990Figure1Population, 10, seed=2)
	S[5, c("x", "y")] <- c(10,6)
	init <- S[, c("x", "y")]
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", 
		initial_sample=init) 
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(11)
	)
})
test_that("createACS, Are y-values of edge units equal to 0?", {
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		unique(Z[which(Z$Sampling == "Edge"), ]$y_value),
		equals(0)
	)
})
test_that("createACS, Are m-values of edge units 0?", {
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		unique(Z[which(Z$Sampling == "Edge"), ]$m),
		equals(0)
	)
})
test_that("createACS, Does the function work when no seed or initial sample is provided?", {
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value")
	expect_gte(
		dim(Z)[1],
		10
	)
})
test_that("createACS, Are there duplicates units in the sample?", {
	Z <- createACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		dim(Z[duplicated(Z), ])[1],
		equals(0)
	)
})
# createRACS
test_that("createRACS, Does the function work when providing the seed and without providing the initial sample? Example 1: no adaptive cluster sampling takes place.", {
	data(Thompson1990Figure1Population)
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", seed=2)
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(0)
	)
})
test_that("createRACS, Does the function work when providing the seed and without providing the initial sample? Example 2: adaptive cluster sampling takes place", {
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(4)
	)
})
test_that("createRACS, Does the function work when providing the initial sample?", {
	S <- createSRS(Thompson1990Figure1Population, 10, seed=2)
	S[5, c("x", "y")] <- c(10,6)
	init <- S[, c("x", "y")]
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", 
		initial_sample=init) 
	expect_that(
		dim(Z[which(Z$y_value > 0), ])[1],
		equals(9)
	)
})
test_that("createRACS, Are y-values of edge units equal to 0?", {
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		unique(Z[which(Z$Sampling == "Edge"), ]$y_value),
		equals(0)
	)
})
test_that("createRACS, Are m-values of edge units 0?", {
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		unique(Z[which(Z$Sampling == "Edge"), ]$m),
		equals(0)
	)
})
test_that("createRACS, Does the function work when no seed or initial sample is provided?", {
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value")
	expect_gte(
		dim(Z)[1],
		10
	)
})
test_that("createRACS, Are there duplicates units in the sample?", {
	Z <- createRACS(Thompson1990Figure1Population, 10, "y_value", seed=26)
	expect_that(
		dim(Z[duplicated(Z), ])[1],
		equals(0)
	)
})