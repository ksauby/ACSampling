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


patch_data_summary_wide <- createWidePopulationSummaryStatistics(
	PopulationSummaryStatistics = patch_data_summary,
	ovar = ovar,
	rvar = rvar
)