test_that("test mChar functions", {
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