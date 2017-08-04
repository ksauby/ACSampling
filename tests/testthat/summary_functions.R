test_that("calculatePopulationSummaryStatistics", {
	CactusRealizationSummary <- createCactusRealizationSummary(CactusRealizations)
	
	# TEST RATIO VARIABLE CALCULATIONS
	# population 1
	pop_1_stricta <- CactusRealizations %>% filter(population==1, Stricta==1)
	mean_pop_1_CACA_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="CACA_on_Stricta") %$% 
		Mean
	mean_pop_1_MEPR_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="MEPR_on_Stricta") %$% 
		Mean
	mean_pop_1_Old_Moth_Evidence_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="Old_Moth_Evidence_Stricta") %$% 
		Mean
	var_pop_1_CACA_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="CACA_on_Stricta") %$% 
		Var
	var_pop_1_MEPR_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="MEPR_on_Stricta") %$% 
		Var
	var_pop_1_Old_Moth_Evidence_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="Old_Moth_Evidence_Stricta") %$% 
		Var
	CV_pop_1_CACA_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="CACA_on_Stricta") %$% 
		CV
	CV_pop_1_MEPR_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="MEPR_on_Stricta") %$% 
		CV
	CV_pop_1_Old_Moth_Evidence_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==1, variable=="Old_Moth_Evidence_Stricta") %$% 
		CV
				
	expect_that(
		mean_pop_1_CACA_on_Stricta,
		equals(mean(pop_1_stricta$CACA_on_Stricta))
	)
	expect_that(
		mean_pop_1_MEPR_on_Stricta,
		equals(mean(pop_1_stricta$MEPR_on_Stricta))
	)
	expect_that(
		mean_pop_1_Old_Moth_Evidence_Stricta,
		equals(mean(pop_1_stricta$Old_Moth_Evidence_Stricta))
	)
	expect_that(
		var_pop_1_CACA_on_Stricta,
		equals(PopVariance(pop_1_stricta$CACA_on_Stricta))
	)
	expect_that(
		var_pop_1_MEPR_on_Stricta,
		equals(PopVariance(pop_1_stricta$MEPR_on_Stricta))
	)
	expect_that(
		var_pop_1_Old_Moth_Evidence_Stricta,
		equals(PopVariance(pop_1_stricta$Old_Moth_Evidence_Stricta))
	)
	expect_that(
		CV_pop_1_CACA_on_Stricta,
		equals(popCV(pop_1_stricta$CACA_on_Stricta))
	)
	expect_that(
		CV_pop_1_MEPR_on_Stricta,
		equals(popCV(pop_1_stricta$MEPR_on_Stricta))
	)
	expect_that(
		CV_pop_1_Old_Moth_Evidence_Stricta,
		equals(popCV(pop_1_stricta$Old_Moth_Evidence_Stricta))
	)
	# population 6
	pop_6_stricta <- CactusRealizations %>% filter(population==6, Stricta==1)
	mean_pop_6_CACA_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="CACA_on_Stricta") %$% 
		Mean
	mean_pop_6_MEPR_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="MEPR_on_Stricta") %$% 
		Mean
	mean_pop_6_Old_Moth_Evidence_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="Old_Moth_Evidence_Stricta") %$% 
		Mean
	var_pop_6_CACA_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="CACA_on_Stricta") %$% 
		Var
	var_pop_6_MEPR_on_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="MEPR_on_Stricta") %$% 
		Var
	var_pop_6_Old_Moth_Evidence_Stricta <- CactusRealizationSummary[[2]] %>% 
		filter(population==6, variable=="Old_Moth_Evidence_Stricta") %$% 
		Var
		
	expect_that(
		mean_pop_6_CACA_on_Stricta,
		equals(mean(pop_6_stricta$CACA_on_Stricta))
	)
	expect_that(
		mean_pop_6_MEPR_on_Stricta,
		equals(mean(pop_6_stricta$MEPR_on_Stricta))
	)
	expect_that(
		mean_pop_6_Old_Moth_Evidence_Stricta,
		equals(mean(pop_6_stricta$Old_Moth_Evidence_Stricta))
	)
	expect_that(
		var_pop_6_CACA_on_Stricta,
		equals(PopVariance(pop_6_stricta$CACA_on_Stricta))
	)
	expect_that(
		var_pop_6_MEPR_on_Stricta,
		equals(PopVariance(pop_6_stricta$MEPR_on_Stricta))
	)
	expect_that(
		var_pop_6_Old_Moth_Evidence_Stricta,
		equals(PopVariance(pop_6_stricta$Old_Moth_Evidence_Stricta))
	)
	
	
})



# ALSO TEST OVAR

test_that("Sampling Bias and Relative Efficiency, population 6, SamplingDesign=ACS, N.SRSWOR.plots==100", {	
	patch_data_summary_wide <- createWidePopulationSummaryStatistics(
		PopulationSummaryStatistics = CactusRealizationSummary,
		ovar = "Stricta",
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	population_6 <- patch_data_summary_wide %>% filter(population==6)
	# MANUALLY CALCULATE MSE and RE
	simulation_data <- simdata_all_re %>% filter(
		population==6, 
		SamplingDesign=="ACS", 
		N.SRSWOR.plots==100
	)
	simulation_data_ratio <- simdata_all_re %>% filter(
		population==6, 
		SamplingDesign=="ACS", 
		N.SRSWOR.plots==100,
		Stricta_mean_observed > 0
	)
	# occupancy variables
	mean_MSE_Stricta <- sum(
		(
			# observed
			simulation_data$Stricta_mean_observed - 
			# true
			population_6$Stricta_mean
		)^2
	# n simulations
	)/dim(simulation_data)[1]
	RE_Stricta = (
		population_6$Stricta_var/unique(simulation_data$N.Total.plots_mean) *
		(1 - unique(simulation_data$N.Total.plots_mean)/population_6$N)
	) /	mean_MSE_Stricta
	# ratio variables
	mean_MSE_CACA_on_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$CACA_on_Stricta_ratio_mean_observed - 
			# true
			population_6$CACA_on_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	mean_MSE_MEPR_on_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$MEPR_on_Stricta_ratio_mean_observed - 
			# true
			population_6$MEPR_on_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	mean_MSE_Old_Moth_Evidence_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$Old_Moth_Evidence_Stricta_ratio_mean_observed - 
			# true
			population_6$Old_Moth_Evidence_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	RE_CACA_on_Stricta = (
		population_6$CACA_on_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_6$N)
	) /	mean_MSE_CACA_on_Stricta
	RE_MEPR_on_Stricta = (
		population_6$MEPR_on_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_6$N)
	) /	mean_MSE_MEPR_on_Stricta
	RE_Old_Moth_Evidence_Stricta = (
		population_6$Old_Moth_Evidence_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_6$N)
	) /	mean_MSE_Old_Moth_Evidence_Stricta
	# CALCULATE MSE and RE USING FUNCTIONS
	example_bias = calculateSamplingBias(
		population_data_summary	= population_6, 
		simulation_data = simulation_data, 
		sampling.grouping.variables	= c("N.Total.plots_mean", "N.SRSWOR.plots", 
			"SamplingDesign"), 
		population.grouping.variables = "population",
		ovar = "Stricta", 
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	RE_values <- calculateRE(
		population_data = population_6,
		MSE_ComparisonSamplingDesign = example_bias,
		population.grouping.variables = "population",
		sample.size.variable = "N.Total.plots_mean",
		ovar = "Stricta",
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	
	# TEST FUNCTION CALCULATIONS	
	# 	occupancy variables
	# 		MSE
	expect_that(
		mean_MSE_Stricta,
		equals(example_bias$Stricta_mean_MSE)
	)
	#		RE
	expect_that(
		RE_Stricta,
		equals(RE_values$Stricta_RE)
	)
	# 	ratio variables
	# 		MSE
	expect_that(
		mean_MSE_CACA_on_Stricta,
		equals(example_bias$CACA_on_Stricta_ratio_mean_MSE)
	)
	expect_that(
		mean_MSE_MEPR_on_Stricta,
		equals(example_bias$MEPR_on_Stricta_ratio_mean_MSE)
	)
	expect_that(
		mean_MSE_Old_Moth_Evidence_Stricta,
		equals(example_bias$Old_Moth_Evidence_Stricta_ratio_mean_MSE)
	)
	#		RE
	expect_that(
		RE_CACA_on_Stricta,
		equals(RE_values$CACA_on_Stricta_ratio_RE)
	)
	expect_that(
		RE_MEPR_on_Stricta,
		equals(RE_values$MEPR_on_Stricta_ratio_RE)
	)
	expect_that(
		RE_Old_Moth_Evidence_Stricta,
		equals(RE_values$Old_Moth_Evidence_Stricta_ratio_RE)
	)
})



test_that("Sampling Bias and Relative Efficiency, population 1, SamplingDesign=ACS, N.SRSWOR.plots==40", {	
	patch_data_summary_wide <- createWidePopulationSummaryStatistics(
		PopulationSummaryStatistics = CactusRealizationSummary,
		ovar = "Stricta",
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	population_1 <- patch_data_summary_wide %>% filter(population==1)
	# MANUALLY CALCULATE MSE and RE
	simulation_data <- simdata_all_re %>% filter(
		population==1, 
		SamplingDesign=="ACS", 
		N.SRSWOR.plots==40
	)
	simulation_data_ratio <- simdata_all_re %>% filter(
		population==1, 
		SamplingDesign=="ACS", 
		N.SRSWOR.plots==40,
		Stricta_mean_observed > 0
	)
	# occupancy variables
	mean_MSE_Stricta <- sum(
		(
			# observed
			simulation_data$Stricta_mean_observed - 
			# true
			population_1$Stricta_mean
		)^2
	# n simulations
	)/dim(simulation_data)[1]
	RE_Stricta = (
		population_1$Stricta_var/unique(simulation_data$N.Total.plots_mean) *
		(1 - unique(simulation_data$N.Total.plots_mean)/population_1$N)
	) /	mean_MSE_Stricta
	# ratio variables
	mean_MSE_CACA_on_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$CACA_on_Stricta_ratio_mean_observed - 
			# true
			population_1$CACA_on_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	mean_MSE_MEPR_on_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$MEPR_on_Stricta_ratio_mean_observed - 
			# true
			population_1$MEPR_on_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	mean_MSE_Old_Moth_Evidence_Stricta <- sum(
		(
			# observed
			simulation_data_ratio$Old_Moth_Evidence_Stricta_ratio_mean_observed - 
			# true
			population_1$Old_Moth_Evidence_Stricta_ratio_mean
		)^2
	# n simulations
	)/dim(simulation_data_ratio)[1]
	RE_CACA_on_Stricta = (
		population_1$CACA_on_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_1$N)
	) /	mean_MSE_CACA_on_Stricta
	RE_MEPR_on_Stricta = (
		population_1$MEPR_on_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_1$N)
	) /	mean_MSE_MEPR_on_Stricta
	RE_Old_Moth_Evidence_Stricta = (
		population_1$Old_Moth_Evidence_Stricta_ratio_var/unique(simulation_data_ratio$N.Total.plots_mean) *
		(1 - unique(simulation_data_ratio$N.Total.plots_mean)/population_1$N)
	) /	mean_MSE_Old_Moth_Evidence_Stricta
	# CALCULATE MSE and RE USING FUNCTIONS
	example_bias = calculateSamplingBias(
		population_data_summary	= population_1, 
		simulation_data = simulation_data, 
		sampling.grouping.variables	= c("N.Total.plots_mean", "N.SRSWOR.plots", 
			"SamplingDesign"), 
		population.grouping.variables = "population",
		ovar = "Stricta", 
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	RE_values <- calculateRE(
		population_data = population_1,
		MSE_ComparisonSamplingDesign = example_bias,
		population.grouping.variables = "population",
		sample.size.variable = "N.Total.plots_mean",
		ovar = "Stricta",
		rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
			"Height_Stricta", "Old_Moth_Evidence_Stricta")
	)
	
	# TEST FUNCTION CALCULATIONS	
	# 	occupancy variables
	# 		MSE
	expect_that(
		mean_MSE_Stricta,
		equals(example_bias$Stricta_mean_MSE)
	)
	#		RE
	expect_that(
		RE_Stricta,
		equals(RE_values$Stricta_RE)
	)
	# 	ratio variables
	# 		MSE
	expect_that(
		mean_MSE_CACA_on_Stricta,
		equals(example_bias$CACA_on_Stricta_ratio_mean_MSE)
	)
	expect_that(
		mean_MSE_MEPR_on_Stricta,
		equals(example_bias$MEPR_on_Stricta_ratio_mean_MSE)
	)
	expect_that(
		mean_MSE_Old_Moth_Evidence_Stricta,
		equals(example_bias$Old_Moth_Evidence_Stricta_ratio_mean_MSE)
	)
	#		RE
	expect_that(
		RE_CACA_on_Stricta,
		equals(RE_values$CACA_on_Stricta_ratio_RE)
	)
	expect_that(
		RE_MEPR_on_Stricta,
		equals(RE_values$MEPR_on_Stricta_ratio_RE)
	)
	expect_that(
		RE_Old_Moth_Evidence_Stricta,
		equals(RE_values$Old_Moth_Evidence_Stricta_ratio_RE)
	)
})
