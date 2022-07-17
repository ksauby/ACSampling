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
      popvar = "pop",
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