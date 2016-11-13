test_that("calculateSamplingBias", {
	# PREP DATA
	data(Thompson1990Figure1Population)
	# calculate y_value estimates
	samplesizes <- data.frame(
		seed = rep(1:6),
		N.Total.plots = rep(seq(from=10, to=60, by=10), each=6)
	)
	y_value_mean_observed <- vector()
		for (i in 1:dim(samplesizes)[1]) {
			temp <- createSRS(
				population=Thompson1990Figure1Population, 
				seed=samplesizes$seed[i], 
				n1=samplesizes$N.Total.plots[i]
			)
			y_value_mean_observed[i] <- mean(temp$y_value)
		}
	data <- cbind(samplesizes, y_value_mean_observed)
	# summarise population data
	Thompson1990Figure1Population_summary <- 
		calculatePopulationSummaryStatistics(
			population_data		= Thompson1990Figure1Population, 
			summary.variables 	= "y_value", 
			grouping.variables 	= NULL, 
			ratio.variables 	= NULL
		)
	# sampling bias
bias <- (data$y_value_mean_observed - Thompson1990Figure1Population_summary$y_value_mean)/Thompson1990Figure1Population_summary$y_value_mean*100
bias %<>% round(0)

	expect_that(
		calculateSamplingBias(
			population_data_summary	= Thompson1990Figure1Population_summary, 
			simulation_data		= data, 
			grouping.variables	= NULL, 
			variables			= "y_value", 
			rvar				= NULL, 
			statistics			= "mean", 
			ratio.statistics	= NULL
		) %$% y_value_mean_bias,
		equals(bias)
	)
})



test_that("calculateMSE", {
	Thompson1990Figure1Population_sampling_bias <- calculateSamplingBias(
		population_data_summary	= Thompson1990Figure1Population_summary, 
		simulation_data		= data, 
		grouping.variables	= NULL, 
		variables			= "y_value", 
		rvar				= NULL, 
		statistics			= "mean", 
		ratio.statistics	= NULL
	)

	MSE_values <- Thompson1990Figure1Population_sampling_bias %>%
		group_by(N.Total.plots) %>%
		summarise(MSE = sum(y_value_mean_MSE_i)/length(y_value_mean_MSE_i)) %$%
		round(MSE, 5)

	expect_that(
		calculateMSE(
			simulation_data_summary = 
				Thompson1990Figure1Population_sampling_bias,
			grouping.variables = "N.Total.plots", 
			variables = "y_value", 
			rvar = NULL,
			statistics = "mean"
		) %$% round(y_value_mean_MSE,5),
		equals(MSE_values)
	)
})