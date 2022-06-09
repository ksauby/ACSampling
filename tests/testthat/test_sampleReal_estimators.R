popdata <- Thompson1990Fig1Pop %>%
   mutate(N = dim(Thompson1990Fig1Pop)[1])
popdata %<>%
   mutate(pop = 1)

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

test_that("test rvarMultDatCalc", {
   dat_results <- rvarMultDatCalc(
      dats,
      rvar = "CACAonStricta",
      N,
      n1
   ) %>%
      mutate(
         CACAonStrictaRMeanObs = round(CACAonStrictaRMeanObs, 5),
         CACAonStrictaRVarObs = round(CACAonStrictaRVarObs, 5)
      )

   expect_equal(
      dat_results,
      data.frame(
         CACAonStrictaRMeanObs = round(c(
            CACAonStrictaRMeanObs_value, CACAonStrictaRMeanObs_value2),5
         ),
         CACAonStrictaRVarObs = c(16.96768, 0.36784),
         Plots = c("R_smd", "R_smd2")
      )
   )
})



