test_that("calculateSamplingBias, bias calculation of mean and variance", {
	# PREP DATA
	data(Thompson1990Figure1Population)
	# calculate y_value estimates
	samplesizes <- data.frame(
		seed = 1:150,
		N.Total.plots = rep(seq(from=130, to=280, by=30), each=25)
	)
	y_value_mean_observed <- vector()
	y_value_var_observed <- vector()
		for (i in 1:dim(samplesizes)[1]) {
			temp <- createSRS(
				population=Thompson1990Figure1Population, 
				seed=samplesizes$seed[i], 
				n1=samplesizes$N.Total.plots[i]
			)
			y_value_mean_observed[i] <- mean(temp$y_value)
			y_value_var_observed[i] <- var(temp$y_value)
		}
	data <- cbind(samplesizes, y_value_mean_observed, y_value_var_observed)
	# summarise population data
	Thompson1990Figure1Population_summary <- 
		calculatePopulationSummaryStatistics(
			population_data		= Thompson1990Figure1Population, 
			summary.variables 	= "y_value", 
			grouping.variables 	= NULL, 
			ratio.variables 	= NULL
		)
	# sampling bias
	data_summary <- data %>%
	group_by(N.Total.plots) %>%
	summarise(
		y_value_mean_RB = (
			# mean of HT estimates
			mean(y_value_mean_observed) - 
			# mu, population mean
			Thompson1990Figure1Population_summary$y_value_mean
		) / Thompson1990Figure1Population_summary$y_value_mean * 100,
		y_value_var_RB = (
			mean(y_value_var_observed) - var(y_value_mean_observed)
		) / var(y_value_mean_observed) * 100,
		y_value_mean_MSE = sum((
			y_value_mean_observed - 
			Thompson1990Figure1Population_summary$y_value_mean
		)^2) / length(y_value_mean_observed)
	) %>%
	as.data.frame
	temp <- data_summary %>% dplyr::select(y_value_mean_RB, y_value_var_RB)

	temp$y_value_mean_RB %<>% round(3)
	temp$y_value_var_RB %<>% round(3)

	data_summary_stats <- calculateSamplingBias(
		population_data_summary	= Thompson1990Figure1Population_summary, 
		simulation_data		= data, 
		population.grouping.variables = NULL, 
		sampling.grouping.variables	= "N.Total.plots", 
		variables			= "y_value", 
		rvar				= NULL 
	) %>%
	as.data.frame

	expect_that(
		data_summary_stats %>%
		dplyr::select(
			y_value_mean_RB,
			y_value_var_RB
		) %>%
		round(3),
		equals(temp)
	)
})

test_that("calculateSamplingBias, MSE function", {

	temp <- data_summary %>% dplyr::select(y_value_mean_MSE)

	temp$y_value_mean_MSE %<>% round(3)

	expect_that(
		data_summary_stats %>%
		dplyr::select(
			y_value_mean_MSE
		) %>%
		round(3),
		equals(temp)
	)
})

test_that("calculateRE", {
	# ACS SAMPLING
	y_value_var_observed <- vector()
	for (i in 1:dim(samplesizes)[1]) {
		temp <- createACS(
			population=Thompson1990Figure1Population, 
			seed=samplesizes$seed[i], 
			n1=samplesizes$N.Total.plots[i],
			y_variable="y_value"
		)
		y_value_mean_observed[i] <- y_HT(
			y = temp$y_value,
			N = dim(Thompson1990Figure1Population)[1],
			n1 = samplesizes$N.Total.plots[i],
			m = temp$m
		)
		y_value_var_observed[i] <- var_y_HT(
			y = temp$y_value,
			N = dim(Thompson1990Figure1Population)[1],
			n1 = samplesizes$N.Total.plots[i],
			m = temp$m
		)
	}
	ACSdata <- cbind(samplesizes, y_value_mean_observed, y_value_var_observed)

	ACSdata_summary_stats <- calculateSamplingBias(
		population_data_summary	= Thompson1990Figure1Population_summary, 
		simulation_data		= ACSdata, 
		population.grouping.variables = NULL, 
		sampling.grouping.variables	= "N.Total.plots", 
		variables			= "y_value", 
		rvar				= NULL 
	) %>%
	as.data.frame
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	y_value_mean_observed <- vector()
		for (i in 1:dim(samplesizes)[1]) {
			
			
		}
	data <- cbind(samplesizes, y_value_mean_observed)

	Thompson1990Figure1Population_ACS_sampling_bias <- calculateSamplingBias(
		population_data_summary	= Thompson1990Figure1Population_summary, 
		simulation_data		= data, 
		grouping.variables	= NULL, 
		variables			= "y_value", 
		rvar				= NULL, 
		statistics			= "mean", 
		ratio.statistics	= NULL
	)

	MSE_ACS_values <- Thompson1990Figure1Population_ACS_sampling_bias %>%
		group_by(N.Total.plots) %>%
		summarise(MSE = sum(y_value_mean_MSE_i)/length(y_value_mean_MSE_i)) %$%
		round(MSE, 5)

	expect_that(
		calculateRE(
			MSE_ComparisonSamplingDesign = MSE_ACS_values,
			MSE_BaselineSamplingDesign = MSE_values,
			grouping.variables = "N.Total.plots"
		) %$% round(y_value_mean_MSE,5),
		equals(MSE_values)
	)
})