test_that("calcSqDiff", {
     dat <- data.frame(
          Var1MeanObs = 1,
          Var1TrueMean = 2,
          Var2MeanObs = 5,
          Var2TrueMean = 10
     )
     Vars = c("Var1", "Var2")
     
     exp_dat <- data.frame(
          dat,
          Var1MeanObs_True2 = 1,
          Var2MeanObs_True2 = 25
     )
     
     expect_equal(
          calcSqDiff(dat, Vars),
          exp_dat
     )
})

test_that("calcDiffMeans", {
     dat <- data.frame(
          Var1MeanObs = 1,
          Var1_MeanObsMeans = 2,
          Var2MeanObs = 5,
          Var2_MeanObsMeans = 10
     )
     Vars = c("Var1", "Var2")
     
     exp_dat <- data.frame(
          dat,
          Var1_Obs_MeanObsMeans = -1,
          Var2_Obs_MeanObsMeans = -5
     )
     
     expect_equal(
          calcDiffMeans(dat, Vars),
          exp_dat
     )
})
# 
# test_that("calcSqDiff", {
#      dat <- data.frame(
#           Var1MeanObs = c(1,2,3,1,2,3),
#           Var1MeanObs_True2 = c(2,2,2,2,2,2),
#           Var1VarObs = c(5,5,5,5,5,5)
#      )
#           
#      exp_dat <- data.frame(
#         Var1TrueMean,
#         Var1MeanRB,
#         Var1MeanMSE,
#         Var1VarRB,
#         Var1MeanRBn,
#         Var1MeanMSEn,
#         Var1VarRBn
#         
#         Var1mean_RB_true_mean = var(dat$Var1TrueMean)
#         Var1MeanRBMeanOfObsMean = mean(dat$Var1MeanObs, na.rm=TRUE)
#         Var1RB_n = length(!is.na(dat$Var1MeanObs))
#         Var1MSESumObs_True2 = sum(!!VarMeanObsMinusTrue2, na.rm = TRUE)
#         Var1MSESampleSize = length(!is.na(!!VarMeanObsMinusTrue2))
#         Var1varRBMeanOfVarEst = mean(!!varVarObs, na.rm = TRUE)
#         Var1varRBMeanOfVarEst_n = length(!is.na(!!varVarObs))
#         Var1varRBVarOfMeanEst = var(!!var_observed, na.rm = TRUE)
#    )
#      
# 
# })
# 
# calcSamplingBias
# popvar	<- c("population")
# sampgroupvar <- c("N.SRSWOR.plots", "SamplingDesign", "Plots")
# 
# 
