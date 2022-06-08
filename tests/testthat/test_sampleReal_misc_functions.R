test_that("test misc samplRealization functions", {
   results3 <- data.frame(temp=NA)
   k=1
   tseed=10302
   n1=5
   realvar = "realization"
   popvar = "n.networks"
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
   P <- exampleCactusPop %>% 
      rbind.fill(exampleCactusPop) %>%
      mutate(
         n.networks = 5,
         realization = 1
      )
   misc_info_results <- results3 %>%
      mutate(
         simulation = k,
         seed = tseed,
         N.ACS.plots = dim(exampleCactusPop)[1] - 5,
         N.Total.plots = dim(exampleCactusPop)[1],
         realvar = 1,
         popvar = 5,
         N.SRSWOR.plots = 5
      )
   expect_equal(
      addMiscInfo(k, tseed, P, exampleCactusPop, n1, realvar, popvar, results3),
      misc_info_results
   )
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