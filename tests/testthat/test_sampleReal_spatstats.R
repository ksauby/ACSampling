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
      getJoinCountTestEst(lambdap_10_tau_25_pop$y_value, lwb),
      joincount.test(as.factor(lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]
   )
   expect_equal(
      getMoranTestEst(lambdap_10_tau_25_pop$y_value, lwb),
      moran.test(lambdap_10_tau_25_pop$y_value, lwb)$estimate[1]
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "W"),
      data.frame(
         JoinCountTest.W = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]),
         MoranI.W = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwb)$estimate[1])
      )
   )
   expect_equal(
      fillSpatStatsNA(lambdap_10_tau_25_pop, "W"),
      data.frame(
         JoinCountTest.W = NA,
         MoranI.W = NA
      )
   )
})
