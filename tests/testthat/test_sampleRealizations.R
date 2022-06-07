# summarizeNetworkInfo
n1_vec = c(20, 40)
N = dim(Thompson1990Fig1Pop)[1]
popdata <- Thompson1990Fig1Pop %>%
   mutate(N = dim(Thompson1990Fig1Pop)[1])
popdata %<>%
   mutate(pop = 1)

# summarize manually
Network1sum <- popdata %>% filter(NetworkID == 1) %$% sum(y_value)
Network2sum <- popdata %>% filter(NetworkID == 2) %$% sum(y_value)
Network3sum <- popdata %>% filter(NetworkID == 3) %$% sum(y_value)
Network4_382sum <- 0

Network20_pi_i = pi_i(N = popdata$N[1], n1 = 20, m_vec = popdata$m)
Network40_pi_i = pi_i(N = popdata$N[1], n1 = 40, m_vec = popdata$m)

results_manual <- popdata %>%
   mutate(
      y_value_network_sum = 0,
      y_value_network_sum = replace(
         y_value_network_sum,
         NetworkID == 1,
         Network1sum
      ),
      y_value_network_sum = replace(
         y_value_network_sum,
         NetworkID == 2,
         Network2sum),
      y_value_network_sum = replace(
         y_value_network_sum,
         NetworkID == 3,
         Network3sum
      ),
      pi_i_n1_20 = Network20_pi_i,
      pi_i_n1_40 = Network40_pi_i
   ) %>%
   select(
      NetworkID,
      m,
      pop,
      x,
      y,
      y_value,
      N,
      y_value_network_sum,
      pi_i_n1_20,
      pi_i_n1_40
   ) %>%
   arrange(NetworkID, x, )

results_w_function <-
   summarizeNetworkInfo(
      popdata,
      vars = "y_value",
      popgroupvar = "pop",
      n1_vec,
      yvar = "y_value"
   ) %>%
   arrange(NetworkID, x, y)

test_that("test summarizeNetworkInfo", {
   expect_equal(
      results_manual,
      results_w_function
   )
})


exampleCactusPop <- data.frame(
   Cactus = c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0),
   Stricta = c(0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
   CACAonStricta = c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
   Sampling = c(
      "SRSWOR",
      "SRSWOR",
      "Cluster",
      "Cluster",
      "Cluster",
      "SRSWOR",
      "SRSWOR",
      "Cluster",
      rep("Edge", 4)
   ),
   NetworkID = c(1, 2, 2, 2, 2, 3, 4, 3, 2, 2, 3, 3),
   m = c(1, 6, 6, 6, 6, 4, 1, 4, 6, 6, 4, 4)
)
n1 = 4
N = 100

test_that("test calcPopSummaryStats", {
   popdata_network_m <- c(6, 11, 4, rep(1, 382 - 4 + 1))
   
   list_1 <- data.frame(
      pop = 1,
      networks_m_min = min(popdata_network_m),
      networks_m_max = max(popdata_network_m),
      networks_m_mean = mean(popdata_network_m),
      networks_m_var = popVar(popdata_network_m),
      n_networks_mGreaterThan1 = 3,
      units_m_mean = mean(popdata$m),
      units_m_var = popVar(popdata$m),
      N = 400
   )
   list_2 <- data.frame(
      variable = "y_value",
      Mean = mean(popdata$y_value),
      Var = popVar(popdata$y_value)
      
   )
   CactusRealizationSummary <- calcPopSummaryStats(
      popdata,
      summaryvar = "y_value",
      popgroupvar = "pop",
      rvar = NULL,
      nrow = 20,
      ncol = 20
   )
   expect_equal(
      CactusRealizationSummary[[1]],
      list_1
   )
   
})
test_that("test yHTMultVarCalc, y_HT_RACS", {
   exampleCactusPop_filtered <- exampleCactusPop %>%
      filter(Sampling != "Edge")
   
   Cactus_and_Stricta_estimates <- data.frame(
      Cactus_yHT = new_y_HT(
         y = exampleCactusPop_filtered$Cactus,
         N = N,
         n1 = n1,
         m_threshold = 2,
         pi_i_values = NULL,
         m_vec = exampleCactusPop_filtered$m,
         sampling = NULL,
         criterion = NULL
      ),
      Stricta_yHT = new_y_HT(
         y = exampleCactusPop_filtered$Stricta,
         N = N,
         n1 = n1,
         m_threshold = 2,
         pi_i_values = NULL,
         m_vec = exampleCactusPop_filtered$m,
         sampling = NULL,
         criterion = NULL
      )
   )
   
   m_threshold = 2
   OAVAR <- syms(c("Cactus", "Stricta"))
   
   yHTMultVarCalc_est <- yHTMultVarCalc(
      alldata = exampleCactusPop,
      OAVAR = OAVAR,
      N = N,
      n1 = n1,
      m_threshold = 2,
      y_HT_formula = "y_HT_RACS"
   )
   
   expect_equal(Cactus_and_Stricta_estimates,
                yHTMultVarCalc_est)
})
test_that("test yHTMultVarCalc, y_HT", {
   exampleCactusPop_filtered <- exampleCactusPop %>%
      filter(Sampling != "Edge")
   
   Cactus_and_Stricta_estimates <- data.frame(
      Cactus_yHT = y_HT(
         y = exampleCactusPop_filtered$Cactus,
         N = N,
         n1 = n1,
         pi_i_values = NULL,
         m_vec = exampleCactusPop_filtered$m,
         sampling = NULL,
         criterion = NULL
      ),
      Stricta_yHT = y_HT(
         y = exampleCactusPop_filtered$Stricta,
         N = N,
         n1 = n1,
         pi_i_values = NULL,
         m_vec = exampleCactusPop_filtered$m,
         sampling = NULL,
         criterion = NULL
      )
   )
   
   mThreshold = 2
   OAVAR <- syms(c("Cactus", "Stricta"))
   
   yHTMultVarCalc_est <- yHTMultVarCalc(
      alldata = exampleCactusPop,
      OAVAR = OAVAR,
      N = N,
      n1 = n1,
      y_HT_formula = "y_HT"
   )
   
   expect_equal(Cactus_and_Stricta_estimates,
                yHTMultVarCalc_est)
})
test_that("test varyMultVarCalc, var_y_HT_RACS", {
   exampleCactusPop_filtered <- exampleCactusPop %>%
      filter(!(is.na(NetworkID))) %>%
      group_by(NetworkID) %>%
      filter(row_number() == 1) %>%
      ungroup()
   
   Cactus_and_Stricta_estimates <- data.frame(
      Cactus_var_yHT_RACS = var_y_HT_RACS(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Cactus,
         m_threshold = 2
      ),
      Stricta_var_yHT_RACS = var_y_HT_RACS(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Stricta,
         m_threshold = 2
      )
   )
   
   mThreshold = 2
   OAVAR <- syms(c("Cactus", "Stricta"))
   
   varyMultVarCalc_est <-
      varyMultVarCalc(
         alldata = exampleCactusPop,
         OAVAR = OAVAR,
         N = N,
         n1 = n1,
         var_formula = "var_y_HT_RACS"
      ) %>%
      as.data.frame
   
   expect_equal(Cactus_and_Stricta_estimates,
                varyMultVarCalc_est)
})
test_that("test varyMultVarCalc, var_y_HT", {
   exampleCactusPop_filtered <- exampleCactusPop %>%
      filter(!(is.na(NetworkID))) %>%
      group_by(NetworkID) %>%
      filter(row_number() == 1) %>%
      ungroup()
   
   Cactus_and_Stricta_estimates <- data.frame(
      Cactus_var_yHT = var_y_HT(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Cactus
      ),
      Stricta_var_yHT = var_y_HT(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Stricta
      )
   )
   
   OAVAR <- syms(c("Cactus", "Stricta"))
   
   varyMultVarCalc_est <-
      varyMultVarCalc(
         alldata = exampleCactusPop,
         OAVAR = OAVAR,
         N = N,
         n1 = n1,
         var_formula = "var_y_HT"
      ) %>%
      as.data.frame
   
   expect_equal(Cactus_and_Stricta_estimates,
                varyMultVarCalc_est)
})
test_that("test varyMultVarCalc, var_pi_i", {
   exampleCactusPop_filtered <- exampleCactusPop %>%
      filter(!(is.na(NetworkID))) %>%
      group_by(NetworkID) %>%
      filter(row_number() == 1) %>%
      ungroup()
   
   Cactus_and_Stricta_estimates <- data.frame(
      Cactus_var_yHT = var_y_HT(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Cactus
      ),
      Stricta_var_yHT = var_y_HT(
         N = N,
         n1 = n1,
         m_vec = exampleCactusPop_filtered$m,
         y_total = exampleCactusPop_filtered$Stricta
      )
   )
   
   OAVAR <- syms(c("Cactus", "Stricta"))
   
   varyMultVarCalc_est <-
      varyMultVarCalc(
         alldata = exampleCactusPop,
         OAVAR = OAVAR,
         N = N,
         n1 = n1,
         var_formula = "var_y_HT"
      ) %>%
      as.data.frame
   
   expect_equal(Cactus_and_Stricta_estimates,
                varyMultVarCalc_est)
})


test_that("test createSample", {
   data(Thompson1990Fig1Pop)
   popdata <- Thompson1990Fig1Pop
   seed <- 10
   n1 <- 10
   yvar <- "y_value"
   
   expect_equal(
      createSample(SamplingDesign = "ACS",
                                popdata, seed, n1, yvar),
      createACS(popdata, n1, yvar, criterion = 0, seed)
   )
   expect_equal(
      createSample(
         SamplingDesign = "RACS",
         popdata,
         seed,
         n1,
         yvar,
         f_max = 2
      ),
      createRACS(
         popdata,
         n1,
         yvar,
         criterion = 0,
         seed,
         f_max = 2
      )
   )
   expect_equal(
      createSample(SamplingDesign = "SRS",
                                popdata, seed, n1, yvar),
      createSRS(popdata, n1, seed)
   )
})

test_that("test createSummaryforVarCalcs", {
   exampleCactusPop <- data.frame(
      Cactus = c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0),
      Stricta = c(0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      CACAonStricta = c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      Sampling = c(
         "SRSWOR",
         "SRSWOR",
         "Cluster",
         "Cluster",
         "Cluster",
         "SRSWOR",
         "SRSWOR",
         "Cluster",
         rep("Edge", 4)
      ),
      NetworkID = c(1, 2, 2, 2, 2, 3, 4, 3, 2, 2, 3, 3),
      m = c(1, 6, 6, 6, 6, 4, 1, 4, 6, 6, 4, 4)
   )
   
   manual_summary <- data.frame(
      NetworkID = c(1, 2, 3, 4),
      CACAonStricta_sum = c(0, 1, 1, 0),
      Cactus_sum = c(0, 4, 2, 0),
      Stricta_sum = c(0, 2, 1, 0),
      m = c(1, 6, 4, 1)
   )
   
   expect_equal(
      createSummaryforVarCalcs(
         exampleCactusPop,
         rvar = "CACAonStricta",
         ovar = c("Cactus", "Stricta")
      ),
      manual_summary
   )
})


N = 100
n1 = 4
rvar = "CACAonStricta"
ovar = "Stricta"

# based on Thompson 2002 example 2, pp. 78-79
CACAonStricta = c(60, 60, 14, 1)
Stricta = c(1, 1, 1, 1)
m = c(5, 5, 2, 1)
pi_i = pi_i(N, n1, m)
pi_ij_values = pi_ij(N, n1, m)
intermed <- data.frame(CACAonStricta, pi_i) %>%
   .[!duplicated(.),]

CACAonStrictaRMeanObs_value <-
   sum(intermed$CACAonStricta/intermed$pi_i)/sum(1/intermed$pi_i)

R_smd <- data.frame(
   CACAonStricta = c(60, 14, 1),
   Stricta = c(1, 1, 1),
   m = c(5, 2, 1)
)

# another fake dataset
CACAonStricta2 = c(6, 6, 4, 1)
Stricta2 = c(1, 1, 1, 1)
m2 = c(3, 3, 2, 1)
pi_i2 = pi_i(N, n1, m2)
pi_ij_values2 = pi_ij(N, n1, m2)
intermed2 <- data.frame(CACAonStricta2, pi_i2) %>%
   .[!duplicated(.),]

CACAonStrictaRMeanObs_value2 <-
   sum(intermed2$CACAonStricta2/intermed2$pi_i2)/sum(1/intermed2$pi_i2)

R_smd2 <- data.frame(
   CACAonStricta = c(6, 4, 1),
   Stricta = c(1, 1, 1),
   m = c(3, 2, 1)
)

dats <- c("R_smd", "R_smd2")

test_that("test rvarMultVarCalc", {
   expect_equal(round(
      rvarMultVarCalc(
         R_smd,
         rvar = "CACAonStricta",
         ovar = c("Cactus", "Stricta"),
         N,
         n1
      ),
      5
   ),
   round(
      data.frame(
         CACAonStrictaRMeanObs = CACAonStrictaRMeanObs_value,
         CACAonStrictaRVarObs = 16.96768
      ),
      5
   ))
})

# test_that("test rvarMultDatCalc", {
#    dat_results <- rvarMultDatCalc(
#       dats,
#       rvar = "CACAonStricta",
#       N,
#       n1
#    ) %>%
#       mutate(
#          CACAonStrictaRMeanObs = round(CACAonStrictaRMeanObs, 5),
#          CACAonStrictaRVarObs = round(CACAonStrictaRVarObs, 5)
#       )
#    
#    expect_equal(
#       dat_results,
#       data.frame(
#          CACAonStrictaRMeanObs = round(c(
#             CACAonStrictaRMeanObs_value, CACAonStrictaRMeanObs_value2),5
#          ),
#          CACAonStrictaRVarObs = c(16.96768, 0.36784),
#          Plots = c("R_smd", "R_smd2")
#       )
#    )
# })
# 
# 
sims = 5
n1_vec = c(5, 10)
population <-
   createPop(
      x_start = 1,
      x_end = 30,
      y_start = 1,
      y_end = 30
   )
#' avar = NULL
ovar = c("Stricta",
         "CACA_on_Stricta")
avar = NULL
data(CactusRealizations)
popdata = CactusRealizations # WHY IS THERE ISLAND=NA
bad_dataframe <- data.frame(x = 1, y = 2)

test_that("test sampleRealizations error handling, handleError_popdata", {
   expect_error(
      sampleRealizations(
         popdata = bad_dataframe,
         sims = sims,
         n1_vec = n1_vec,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus"
      ),
      "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'."
   )
})
test_that("test sampleRealizations error handling, handleError_n1vector", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = "a",
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus"
      ),
      "The argument 'n1' must be an integer value or vector of integers."
   )
   # expect_silent(
   #         sampleRealizations(
   #                 popdata = popdata,
   #                 sims = sims,
   #                 n1_vec = 1,
   #                 avar = NULL,
   #                 ovar = ovar,
   #                 popvar="Island",
   #                 yvar="Cactus"
   #         )
   # )
})
test_that("test sampleRealizations error handling, handleError_yvar", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = 15
      ),
      "The argument 'yvar' must be a character string."
   )
})
test_that(
   "test sampleRealizations errors, handleError_LogicalVar, SampleEstimators", {
      expect_error(
         sampleRealizations(
            popdata = popdata,
            sims = sims,
            n1_vec = 1,
            avar = avar,
            ovar = ovar,
            popvar = "Island",
            yvar = "Cactus",
            SampleEstimators = "fake"
         ),
         "The argument 'SampleEstimators' must be assigned a value of either TRUE or FALSE."
      )
   }
)
test_that("test sampleRealizations errors, handleError_LogicalVar, SpatStat", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         SpatStat = "fake"
      ),
      "The argument 'SpatStat' must be assigned a value of either TRUE or FALSE."
   )
})
test_that("test sampleRealizations errors, SamplingDesign", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         SamplingDesign = "GRTS"
      ),
      "SamplingDesign must be supplied as either 'SRS', ACS', or 'RACS'."
   )
})
test_that("test sampleRealizations errors, y_HT_formula", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         y_HT_formula = "yHT"
      ),
      "y_HT_formula must be supplied as either 'y_HT' or 'y_HT_RACS'."
   )
})
test_that("test sampleRealizations errors, var_formula", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         var_formula = "varyHT"
      ),
      "var_formula must be supplied as either 'var_y_HT' or 'var_y_HT_RACS'."
   )
})
test_that("test sampleRealizations errors, weights", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         weights = "We"
      ),
      "weights must be supplied as 'W', 'B', 'C', 'U', or 'S'."
   )
   
})
# test_that("test sampleRealizations errors, popvar", {
#         expect_error(
#                 sampleRealizations(
#                         popdata = popdata,
#                         sims = sims,
#                         n1_vec = 1,
#                         avar = avar,
#                         ovar = ovar,
#                         popvar="Island",
#                         yvar="Cactus",
#                         popvar=5
#                 ),
#                 "The argument popvar must be a character string."
#         )
#
# })
test_that("test sampleRealizations errors, realvar", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         realvar = 5
      ),
      "The argument realvar must be a character string."
   )
   
})
test_that("test sampleRealizations", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         realvar = 5
      ),
      "The argument realvar must be a character string."
   )
   
})        




# 
# sims=20
# n1_vec=c(5,10,20,40)
# population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
# avar = NULL
# ovar = c(
#    "Stricta",
#    "Pusilla",
#    "Cactus",
#    "CACA_on_Pusilla",
#    "CACA_on_Stricta",
#    "MEPR_on_Pusilla",
#    "MEPR_on_Stricta",
#    "Old_Moth_Evidence_Pusilla",
#    "Old_Moth_Evidence_Stricta"
# )
# rvar=NULL
# SamplingDesign="ACS"
# y_HT_formula = "y_HT"
# var_formula = "var_y_HT"
# mThreshold = NULL
# f_max = 2
# SampleEstimators = FALSE
# SpatStat = TRUE
# mChar = TRUE
# popvar = "n.networks"
# realvar = "realization"
# weights="S"
# data(CactusRealizations)
# popdata = CactusRealizations # WHY IS THERE ISLAND=NA
# simulation_data <- sampleRealizations(
#    popdata = popdata,
#    sims = sims,
#    n1_vec = n1_vec,
#    avar = avar,
#    ovar = ovar,
#    popvar="Island",
#    yvar="Cactus"
# )
# sims=200
# n1_vec=c(75,150,225,300,350)
# simulation_data_SRSWOR <- sampleRealizations(
#    popdata = popdata,
#    sims = sims,
#    n1_vec = n1_vec,
#    avar = avar,
#    ovar = ovar,
#    popvar="Island"
# )
# 


test_that("test prepDatasets", {
   testdat <- data.frame(
      x = c(14,16,16,22,22),
      y = c(27, 13, 15, 16, 20),
      Sampling = c(
         "SRSWOR",
         "SRSWOR",
         "Edge",
         "Cluster",
         "Cluster"
      )
   )
   
   testdat_ACS_alldata <- data.frame(
      x = c(14,16,22,22),
      y = c(27,13,16,20),
      Sampling = c(
         "SRSWOR",
         "SRSWOR",
         "Cluster",
         "Cluster"
      )
   )
   testdat_ACS_SRSWOR_data <- data.frame(
      x = c(14,16),
      y = c(27, 13),
      Sampling = c(
         "SRSWOR",
         "SRSWOR"
      )
   )
   testdat_ACS_dats <- c("SRSWOR_data", "alldata")
   expect_equal(
      prepDatasets(SamplingDesign="ACS", testdat),
      list(
         testdat_ACS_SRSWOR_data,
         testdat_ACS_alldata,
         testdat_ACS_dats
      )
   )
   
   testdat_RACS_alldata <- data.frame(
      x = c(14,16,22,22),
      y = c(27,13,16,20),
      Sampling = c(
         "SRSWOR",
         "SRSWOR",
         "Cluster",
         "Cluster"
      )
   )
   testdat_RACS_SRSWOR_data <- data.frame(
      x = c(14,16),
      y = c(27, 13),
      Sampling = c(
         "SRSWOR",
         "SRSWOR"
      )
   )
   testdat_RACS_dats <- c("SRSWOR_data", "alldata")
   expect_equal(
      prepDatasets(SamplingDesign="RACS", testdat),
      list(
         testdat_RACS_SRSWOR_data,
         testdat_RACS_alldata,
         testdat_RACS_dats
      )
   )

   
   testdat_SRSWOR_dats <- "alldata"
   expect_equal(
      prepDatasets(SamplingDesign="SRSWOR", testdat),
      list(
         NA,
         testdat,
         testdat_SRSWOR_dats
      )
   )

})
test_that("test spatial statistics functions", {

   lambdap_10_tau_25_pop <- expand.grid(
      x = 1:10,
      y = 1:10
   ) %>%
      cbind(
         y_value = c(
            rep(0,5),1,1,0,0,0,
            0,0,rep(1,5),rep(0,3),
            
            0,0,rep(1,6),0,1,
            0,0,0,0,rep(1,3),0,1,1,
            
            0,0,0,0,rep(1,5),0,
            1,0,0,rep(1,8),0,0,1,1,0,0,0,0,1,
            
            1,rep(0,9),
            1,1,0,1,1,1,1,1,0,0,
            
            1,0,0,1,1,1,1,1,0,0
         ),
         NetworkID = c(
            1:5, 6,6, 7:9,
            10:11,rep(12,5), NA, 13:14,
            
            NA,15,rep(16,6), 17, 18,
            19:22,rep(23,3),NA,24,24,
            
            NA,NA,NA,25, rep(26,5), NA,
            27:29, rep(30,8), 31,32,33,33,34,35,36,37,38,
            
            39, NA, 40:47,
            48,48, 49, rep(50, 5), NA, 51,
            52,53,54, rep(55,5), 56, NA
         )
      ) %>%
      #filter(!(is.na(NetworkID))) %>%
      arrange(x, y)
   
   nb <- cell2nb(
      nrow = max(lambdap_10_tau_25_pop$x) - min(lambdap_10_tau_25_pop$x) + 1, 
      ncol = max(lambdap_10_tau_25_pop$y) - min(lambdap_10_tau_25_pop$y) + 1
   )
   coordinates(lambdap_10_tau_25_pop) = ~ x+y
   
   data_dist <- dim(as.matrix(dist(cbind(lambdap_10_tau_25_pop$x, lambdap_10_tau_25_pop$y))))[1]
   tempdat <- data.frame(JoinCountTest.W = NA)
   weight = "W"
   lwb <- nb2listw(nb, style = weight) # convert to neighbor list to weights list
   
   expect_equal(
      getJoinCountTestEst(lambdap_10_tau_25_pop, lwb),
      joincount.test(as.factor(lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]
   )
   
   expect_equal(
      getMoranTestEst(lambdap_10_tau_25_pop, lwb),
      moran.test(lambdap_10_tau_25_pop$y_value, lwb)$estimate[1]
   )
   
   
   
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "W"),
      data.frame(
         JoinCountTest.W = as.vector(joincount.test(as.factor(lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]),
         MoranI.W = as.vector(moran.test(lambdap_10_tau_25_pop$y_value, lwb)$estimate[1])
      )
   )
   
   expect_equal(
      fillSpatStatsNA(lambdap_10_tau_25_pop, "W"),
      data.frame(
         JoinCountTest.W = NA,
         MoranI.W = NA
      )
   )
   
   datnew <- data.frame(temp = NA)
   
   expect_equal(
      fillmCharNA(datnew),
      data.frame(
         temp=NA,
         mean_m=NA,
         median_m=NA,
         max_m=NA,
         min_m=NA,
         mean_uniq_m=NA,
         median_uniq_m=NA,
         max_uniq_m=NA,
         min_uniq_m=NA
      )
   )
   
   
   
   exampleCactusPop <- data.frame(
      Cactus = c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0),
      Stricta = c(0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      CACAonStricta = c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      Sampling = c(
         "SRSWOR", "SRSWOR", "Cluster", "Cluster", "Cluster", "SRSWOR",
         "SRSWOR", "Cluster", rep("Edge", 4)
      ),
      NetworkID = c(1, 2, 2, 2, 2, 3, 4, 3, 2, 2, 3, 3),
      m = c(1, 6, 6, 6, 6, 4, 1, 4, 6, 6, 4, 4)
   )
   temp <- exampleCactusPop %>%
      dplyr::filter(Cactus > 0)
   results <- data.frame(temp = NA)
   results2 <- data.frame(temp = NA)
   results2$mean_m <- mean(temp$m)
   results2$median_m <- median(temp$m)
   results2$max_m <- max(temp$m)
   results2$min_m <- min(temp$m)
   results2$mean_uniq_m = mean(unique(temp$m))
   results2$median_uniq_m = median(unique(temp$m))
   results2$max_uniq_m = max(unique(temp$m))
   results2$min_uniq_m = min(unique(temp$m))
   
   
   expect_equal(
      fillmChar(temp, results, "Cactus"),
      results2
   )
   
})



# 
# test_that("test sampleRealizations", {
#    expect_error(
#       sampleRealizations(
#          popdata = popdata,
#          sims = sims,
#          n1_vec = 1,
#          avar = avar,
#          ovar = ovar,
#          popvar = "Island",
#          yvar = "Cactus",
#          realvar = 5
#       ),
#       "The argument realvar must be a character string."
#    )
#    
# })        
# 
# 

#
#
#
# data(Thompson1990Fig1Pop)
# alldata_all <- createACS(Thompson1990Fig1Pop, 20, "y_value", seed=24)
#
# temp <- alldata_all %>%
#         as.data.frame %>%
#         # get rid of edge units - not involved in calculation of m
#         filter(!(is.na(NetworkID))) %>%
#         arrange(x, y)
#
# # dnearneigh - why was this here?
#
# nb <- cell2nb(
#         nrow = max(temp$x) - min(temp$x) + 1,
#         ncol = max(temp$y) - min(temp$y) + 1
# )
# coordinates(temp) = ~ x+y
# data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
# tempdat <- data.frame(JoinCountTest.W = NA)
# for (i in length(weights)) {
#         lwb <- nb2listw(nb, style = weights[i]) # convert to weights
#         # I think cells are indexed by row, then column
#         tempdat$JoinCountTest <- getJoinCountTestEst(temp, lwb)
#         tempdat$MoranI <- getMoranTestEst(temp, lwb)
#         colnames(tempdat)[which(names(tempdat) == "JoinCountTest")] <-
#                 paste("JoinCountTest", weights[i], sep=".")
#         colnames(tempdat)[which(names(tempdat) == "MoranI")] <-
#                 paste("MoranI", weights[i], sep=".")
# }
# return(tempdat)