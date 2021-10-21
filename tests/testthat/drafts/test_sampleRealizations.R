dataset = createACS(popdata=PlotSurveys_season1, seed=9, n1=10, yvar="Cactus", condition=0) %>% as.data.frame

dataset <- data.frame(
     y_stricta = c(60, 14, 1), 
     stricta = c(1, 1, 1),
     m = c(5, 2, 1)
)
dataset_results <- data.frame(Var1 = NA)
calcRatioEst(
     dataset, 
     dataset_results, 
     rvar="y_stricta", 
     N=100, 
     n1=4
)



expect_equal(
     round(
          R_hat(
               , 
               N = 100, 
               n1 = 4, 
               , 
               replace="TRUE"
          ), 2
     ),
     12.12
)