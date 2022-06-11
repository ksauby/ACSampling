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
   #coordinates(lambdap_10_tau_25_pop) = ~ x+y
   data_dist <- dim(as.matrix(dist(cbind(lambdap_10_tau_25_pop$x, lambdap_10_tau_25_pop$y))))[1]
   tempdat <- data.frame(JoinCountTest.W = NA)
   lwb <- nb2listw(nb, style = "W") # convert to neighbor list to weights list
   lwbB <- nb2listw(nb, style = "B") # convert to neighbor list to weights list
   lwbC <- nb2listw(nb, style = "C") # convert to neighbor list to weights list
   lwbU <- nb2listw(nb, style = "U") # convert to neighbor list to weights list
   lwbS <- nb2listw(nb, style = "S") # convert to neighbor list to weights list
   lwbminmax <- nb2listw(nb, style = "minmax") # convert to neighbor list to weights list

   expect_equal(
      getJoinCountTestEst(lambdap_10_tau_25_pop$y_value, lwb),
      joincount.test(as.factor(lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]
   )
   expect_equal(
      getMoranTestEst(lambdap_10_tau_25_pop$y_value, lwb),
      moran.test(lambdap_10_tau_25_pop$y_value, lwb)$estimate[1]
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "W", "y_value"),
      data.frame(
         JoinCountTest.W = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwb)[[2]]$estimate[1]),
         MoranI.W = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwb)$estimate[1])
      )
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "B", "y_value"),
      data.frame(
         JoinCountTest.B = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwbB)[[2]]$estimate[1]),
         MoranI.B = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwbB)$estimate[1])
      )
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "C", "y_value"),
      data.frame(
         JoinCountTest.C = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwbC)[[2]]$estimate[1]),
         MoranI.C = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwbC)$estimate[1])
      )
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "U", "y_value"),
      data.frame(
         JoinCountTest.U = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwbU)[[2]]$estimate[1]),
         MoranI.U = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwbU)$estimate[1])
      )
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "S", "y_value"),
      data.frame(
         JoinCountTest.S = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwbS)[[2]]$estimate[1]),
         MoranI.S = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwbS)$estimate[1])
      )
   )
   expect_equal(
      calcSpatStats(lambdap_10_tau_25_pop, "minmax", "y_value"),
      data.frame(
         JoinCountTest.minmax = as.vector(joincount.test(as.factor(
            lambdap_10_tau_25_pop$y_value), lwbminmax)[[2]]$estimate[1]),
         MoranI.minmax = as.vector(moran.test(
            lambdap_10_tau_25_pop$y_value, lwbminmax)$estimate[1])
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
test_that("test calcSpatStat with more complicated example", {
   n1_vec=10
   population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
   avar = NULL
   ovar = c(
      "Stricta",
      "Pusilla",
      "Cactus"
   )
   yvar="Cactus"
   SamplingDesign="ACS"
   y_HT_formula = "y_HT"
   var_formula = "var_y_HT"
   mThreshold = NULL
   f_max = 2
   SampleEstimators = FALSE
   SpatStat = TRUE
   mChar = TRUE
   popvar = "n.networks"
   realvar = "realization"
   weights="S"
   #seeds = 1:1000
   data(CactusRealizations)
   
   popdata <- CactusRealizations %>%
      filter(n.networks == unique(.data$n.networks)[6])
   
   POPVAR <- sym(popvar)
   REALVAR <- sym(realvar)
   n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
   popdata %<>% arrange_at(c(popvar, realvar))
   n.patches <- length(unique(eval(parse(text=paste(
      "popdata$", popvar, sep="")))))
   A <- vector("list", n.patches)
   # c() - same code calculates the HT estimators for occupancy and abundance

   P <- popdata %>% 
      filter(!!POPVAR == unique(eval(parse(text=paste(
         "popdata$", popvar, sep=""
      ))))[1])
   N <- dim(P)[1]
   n1 <- n1_vec[1]
   A[[1]][[1]] <- list()
   tseed1 <- 1
   set.seed(tseed1)
   tseed2 <- runif(1)
   set.seed(tseed2)
   alldata <- ACSampling:::createSample(SamplingDesign, popdata, tseed2, n1, yvar, f_max)
   alldata_all <- alldata
   alldata %<>% filter(Sampling!="Edge")
   
   temp <- alldata_all %>%
      as.data.frame %>%
      filter(!(is.na(NetworkID))) %>%
      arrange(x, y)
   ROW = max(temp$x) - min(temp$x)
   COL = max(temp$y) - min(temp$y)
   nb <- cell2nb(
      nrow = ROW + 1, 
      ncol = COL + 1
   )
   complete.grid <- expand.grid(
      x = min(temp$x):max(temp$x),
      y = min(temp$y):max(temp$y),
      newval = 0
   ) %>%
      as.data.frame() %>%
      merge(
         temp %>% select(x, y, Cactus),
         by=c("x", "y"),
         all=T
      ) %>%
      mutate(
         Cactus = ifelse(
            !is.na(Cactus),
            Cactus,
            newval
         )
      ) %>%
      dplyr::select(-newval)
   tempdat <- data.frame(JoinCountTest = NA)
   lwb <- nb2listw(nb, style = "S") # convert to neighbor list to weights list
      
   
   expect_equal(
      data.frame(
         JoinCountTest.S = as.vector(
            joincount.test(as.factor(complete.grid$Cactus), lwb)[[2]]$estimate[1]),
         MoranI.S = as.vector(moran.test(complete.grid$Cactus, lwb)$estimate[1])
      ),
      ACSampling:::calcSpatStats(alldata_all, weights, yvar)
   )
})