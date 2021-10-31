test_that("test summarizeNetworkInfo", {
        # summarizeNetworkInfo
        n1_vec=c(20,40)
        N = dim(Thompson1990Fig1Pop)[1]
        popdata <- Thompson1990Fig1Pop %>%
                mutate(N= dim(Thompson1990Fig1Pop)[1])
        popdata %<>%
                mutate(
                        pop = 1
                )
        
        # summarize manually
        Network1sum <- popdata %>% filter(NetworkID==1) %$% sum(y_value)
        Network2sum <- popdata %>% filter(NetworkID==2) %$% sum(y_value)
        Network3sum <- popdata %>% filter(NetworkID==3) %$% sum(y_value)
        Network4_382sum <- 0
        
        Network20_pi_i = pi_i(N=popdata$N, n1=20, m_vec=popdata$m)
        Network40_pi_i = pi_i(N=popdata$N, n1=40, m_vec=popdata$m)
        
        results_manual <- popdata %>%
                mutate(
                        y_value_network_sum = 0,
                        y_value_network_sum = replace(
                                y_value_network_sum,
                                NetworkID==1,
                                Network1sum
                        ),
                        y_value_network_sum = replace(
                                y_value_network_sum,
                                NetworkID==2,
                                Network2sum
                        ),
                        y_value_network_sum = replace(
                                y_value_network_sum,
                                NetworkID==3,
                                Network3sum
                        ),
                        pi_i_n1_20 = Network20_pi_i,
                        pi_i_n1_40 = Network40_pi_i
                )
        
        results_w_function <- summarizeNetworkInfo(popdata, vars="y_value", popgroupvar="pop", n1_vec, yvar="y_value") 
        
        expect_equal(
                results_manual,
                results_w_function
        )
        
})


exampleCactusPop <- data.frame(
        Cactus=c(0,1,1,1, 1,1,0,1, 0,0,0,0), 
        Stricta=c(0,1,1,0, 0,1,0,0, 0,0,0,0),
     CACAonStricta=c(0,0,1,0, 0,1,0,0, 0,0,0,0),
     Sampling=c(
          "SRSWOR","SRSWOR","Cluster","Cluster", 
          "Cluster","SRSWOR","SRSWOR","Cluster", 
          rep("Edge", 4)
     ),
     NetworkID=c(1,2,2,2, 2,3,4,3, 2,2,3,3),
     m=c(1,6,6,6, 6,4,1,4, 6,6,4,4)
)
n1=4
N=100

test_that("test calc_y_HT_MultipleVars, y_HT_RACS", {
     exampleCactusPop_filtered <- exampleCactusPop %>%
          filter(Sampling!="Edge")
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_yHT = new_y_HT(
               y=exampleCactusPop_filtered$Cactus, 
               N=N, 
               n1=n1, 
               m_threshold=2, 
               pi_i_values=NULL, 
               m_vec=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          ),
          Stricta_yHT = new_y_HT(
               y=exampleCactusPop_filtered$Stricta, 
               N=N, 
               n1=n1, 
               m_threshold=2, 
               pi_i_values=NULL, 
               m_vec=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          )
     )
     
     mThreshold=2
     OAVAR <- syms(c("Cactus", "Stricta"))
     
     calc_y_HT_MultipleVars_est <- calc_y_HT_MultipleVars(
          alldata=exampleCactusPop, 
          OAVAR=OAVAR, 
          N=N, 
          n1=n1,
          mThreshold=2,
          y_HT_formula="y_HT_RACS"
     )
     
     expect_equal(
          Cactus_and_Stricta_estimates,
          calc_y_HT_MultipleVars_est
     )
})
test_that("test calc_y_HT_MultipleVars, y_HT", {
     exampleCactusPop_filtered <- exampleCactusPop %>%
          filter(Sampling!="Edge")
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_yHT = y_HT(
               y=exampleCactusPop_filtered$Cactus, 
               N=N, 
               n1=n1, 
               pi_i_values=NULL, 
               m_vec=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          ),
          Stricta_yHT = y_HT(
               y=exampleCactusPop_filtered$Stricta, 
               N=N, 
               n1=n1, 
               pi_i_values=NULL, 
               m_vec=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          )
     )
     
     mThreshold=2
     OAVAR <- syms(c("Cactus", "Stricta"))
     
     calc_y_HT_MultipleVars_est <- calc_y_HT_MultipleVars(
          alldata=exampleCactusPop, 
          OAVAR=OAVAR, 
          N=N, 
          n1=n1,
          y_HT_formula="y_HT"
     )
     
     expect_equal(
          Cactus_and_Stricta_estimates,
          calc_y_HT_MultipleVars_est
     )
})

test_that("test calc_var_y_HT_MultipleVars, var_y_HT_RACS", {
     
     exampleCactusPop_filtered <- exampleCactusPop %>%
          filter(!(is.na(NetworkID))) %>%
          group_by(NetworkID) %>%
          filter(row_number()==1) %>%
          ungroup()
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_var_yHT_RACS = var_y_HT_RACS(
               N=N, 
               n1=n1, 
               m_vec=exampleCactusPop_filtered$m,
               y_total=exampleCactusPop_filtered$Cactus, 
               m_threshold=2
          ),
          Stricta_var_yHT_RACS = var_y_HT_RACS(
               N=N, 
               n1=n1, 
               m_vec=exampleCactusPop_filtered$m,
               y_total=exampleCactusPop_filtered$Stricta, 
               m_threshold=2
          )
     )
     
     mThreshold=2
     OAVAR <- syms(c("Cactus", "Stricta"))
     
     calc_var_y_HT_MultipleVars_est <- calc_var_y_HT_MultipleVars(
          alldata=exampleCactusPop, 
          OAVAR=OAVAR, 
          N=N, 
          n1=n1,
          var_formula="var_y_HT_RACS"
     ) %>%
          as.data.frame
     
     expect_equal(
          Cactus_and_Stricta_estimates,
          calc_var_y_HT_MultipleVars_est
     )
})
test_that("test calc_var_y_HT_MultipleVars, var_y_HT", {

exampleCactusPop_filtered <- exampleCactusPop %>%
     filter(!(is.na(NetworkID))) %>%
          group_by(NetworkID) %>%
          filter(row_number()==1) %>%
          ungroup()
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_var_yHT = var_y_HT(
               N=N, 
               n1=n1, 
               m_vec=exampleCactusPop_filtered$m,
               y_total=exampleCactusPop_filtered$Cactus
          ),
          Stricta_var_yHT = var_y_HT(
               N=N, 
               n1=n1, 
               m_vec=exampleCactusPop_filtered$m,
               y_total=exampleCactusPop_filtered$Stricta
          )
     )
     
     OAVAR <- syms(c("Cactus", "Stricta"))
     
     calc_var_y_HT_MultipleVars_est <- calc_var_y_HT_MultipleVars(
          alldata=exampleCactusPop, 
          OAVAR=OAVAR, 
          N=N, 
          n1=n1,
          var_formula="var_y_HT"
     ) %>%
          as.data.frame
     
     expect_equal(
          Cactus_and_Stricta_estimates,
          calc_var_y_HT_MultipleVars_est
     )
})
test_that("test calc_var_y_HT_MultipleVars, var_pi_i", {
        
        exampleCactusPop_filtered <- exampleCactusPop %>%
                filter(!(is.na(NetworkID))) %>%
                group_by(NetworkID) %>%
                filter(row_number()==1) %>%
                ungroup()
        
        Cactus_and_Stricta_estimates <- data.frame(
                Cactus_var_yHT = var_y_HT(
                        N=N, 
                        n1=n1, 
                        m_vec=exampleCactusPop_filtered$m,
                        y_total=exampleCactusPop_filtered$Cactus
                ),
                Stricta_var_yHT = var_y_HT(
                        N=N, 
                        n1=n1, 
                        m_vec=exampleCactusPop_filtered$m,
                        y_total=exampleCactusPop_filtered$Stricta
                )
        )
        
        OAVAR <- syms(c("Cactus", "Stricta"))
        
        calc_var_y_HT_MultipleVars_est <- calc_var_y_HT_MultipleVars(
                alldata=exampleCactusPop, 
                OAVAR=OAVAR, 
                N=N, 
                n1=n1,
                var_formula="var_y_HT"
        ) %>%
                as.data.frame
        
        expect_equal(
                Cactus_and_Stricta_estimates,
                calc_var_y_HT_MultipleVars_est
        )
})


# THIS DOES NOT PASS TESTING
# test_that("test calc_y_HT_MultipleVars, y_HT", {
#         exampleCactusPop_filtered <- exampleCactusPop %>%
#                 filter(Sampling != "Edge")
#         mvals <- exampleCactusPop %>%
#                 group_by(NetworkID) %>%

#                 summarise(m = m[1])
#         R_smd <- exampleCactusPop %>%
#                 filter(Sampling!="Edge") %>%
#                 select("CACAonStricta", "Stricta", "NetworkID") %>%
#                 group_by(NetworkID) %>%
#                 summarise_all(
#                         list(sum = sum),
#                         na.rm = T
#                 ) %>%
#                 merge(mvals, by="NetworkID")
#         Cactus_and_Stricta_estimates <- data.frame(
#                 CACAonStrictaRMeanObs = R_hat(y=y, x=x, N=N, n1=n1, m_vec=R_smd$m),
#                 CACAonStrictaRVarObs = var_R_hat(y=y, x=x, N=N, n1=n1,m_vec=R_smd$m)
#         )
#         calc_rvar_MultipleVars_est <- calc_rvar_MultipleVars(
#                 alldata=exampleCactusPop, 
#                 rvar="CACAonStricta",
#                 ovar="Stricta",
#                 N=N, 
#                 n1=n1
#         )
#         expect_equal(
#                 Cactus_and_Stricta_estimates,
#                 calc_y_HT_MultipleVars_est
#         )
# })


test_that("test createSample", {
        
        data(Thompson1990Fig1Pop)
        popdata <- Thompson1990Fig1Pop
        seed <- 10
        n1 <- 10
        yvar <- "y_value"

        expect_equal(
                ACSampling:::createSample(
                        SamplingDesign="ACS", 
                        popdata, seed, n1, yvar),
                createACS(popdata, n1, yvar, criterion=0, seed)
        )
        expect_equal(
                ACSampling:::createSample(
                        SamplingDesign="RACS", 
                        popdata, seed, n1, yvar, f_max=2),
                createRACS(popdata, n1, yvar, criterion=0, seed, f_max=2)
        )
        expect_equal(
                ACSampling:::createSample(
                        SamplingDesign="SRS", 
                        popdata, seed, n1, yvar),
                createSRS(popdata, n1, seed)
        )
})
# 
# 
# 
# getJoinCountTestEst <- function(temp, lwb) 
# 
# getMoranTestEst <- function(temp, lwb) 
# 
# calcSpatStats <- function(alldata_all, weights) 
# fillSpatStatsNA <- function(alldata_all, weights)
# 
# calcRatioEst <- function(dats, rvar, y, x, N, n1, m) 



test_that("test createSummaryforVarCalcs", {
        
        exampleCactusPop <- data.frame(
                Cactus=c(0,1,1,1, 1,1,0,1, 0,0,0,0), 
                Stricta=c(0,1,1,0, 0,1,0,0, 0,0,0,0),
                CACAonStricta=c(0,0,1,0, 0,1,0,0, 0,0,0,0),
                Sampling=c(
                        "SRSWOR","SRSWOR","Cluster","Cluster", 
                        "Cluster","SRSWOR","SRSWOR","Cluster", 
                        rep("Edge", 4)
                ),
                NetworkID=c(1,2,2,2, 2,3,4,3, 2,2,3,3),
                m=c(1,6,6,6, 6,4,1,4, 6,6,4,4)
        )
        
        manual_summary <- data.frame(
                NetworkID = c(1,2,3,4),
                CACAonStricta_sum = c(0,1,1,0),
                Cactus_sum = c(0,4,2,0),
                Stricta_sum = c(0,2,1,0),
                m = c(1,6,4,1)
        )
        
        expect_equal(
                createSummaryforVarCalcs(exampleCactusPop, rvar="CACAonStricta", ovar=c("Cactus", "Stricta")),
                manual_summary
        )
})




test_that("test calc_rvar_MultipleVars", {
        N = 100
        n1 = 4
        R_smd <- data.frame(
                CACAonStricta = c(60, 14, 1),
                Stricta = c(1, 1, 1),
                m = c(5, 2, 1)
        )
        rvar="CACAonStricta"
        ovar="Stricta"

        # manually calculate R_hat for CACAonStricta and Stricta         
        #       CACAonStricta R_hat mean
        y = c(60, 60, 14, 1)
        x = c(1, 1, 1, 1)
        N = 100
        n1 = 4
        m = c(5, 5, 2, 1)
        pi_i_vals = pi_i(N, n1, m)
        pi_ij_values = pi_ij(N, n1, m)
        IN <- data.frame(y, pi_i_vals) %>%
                .[!duplicated(.),]
        CACAonStrictaRMeanObs_value <- sum(IN$y/IN$pi_i_vals)/sum(1/IN$pi_i_vals)
        #       CACAonStricta R_hat var
        IV <- IN %>%
                mutate(
                        y_hat = y - CACAonStrictaRMeanObs_value,
                        y_hat_sq = y_hat^2,
                        first_sum = ((1/(pi_i_vals^2)) - (1/pi_i_vals)) * y_hat_sq
                )
        
        
        # IV$pi_i_vals[1] + IV$pi_i_vals[2] - (1-(1-(5/100)-(2/100))^4)
        # 
        # 
        # var_est <- sum(IV$first_sum) +
        # (
        #         1/(IV$pi_i_vals[1]*IV$pi_i_vals[2]) - 
        #         1/pi_ij_values[1,2]
        # ) * IV$y_hat[1]*IV$y_hat[2] +
        # (
        #         1/(IV$pi_i_vals[1]*IV$pi_i_vals[3]) - 
        #         1/pi_ij_values[1,3]
        # ) * IV$y_hat[1]*IV$y_hat[3] +
        # (
        #         1/(IV$pi_i_vals[3]*IV$pi_i_vals[2]) - 
        #         1/pi_ij_values[3,2]
        # ) * IV$y_hat[3]*IV$y_hat[2]
        # 
        # 
        expect_equal(
                round(calc_rvar_MultipleVars(
                        R_smd, 
                        rvar="CACAonStricta", 
                        ovar=c("Cactus", "Stricta"),
                        N, n1
                ),5),
                round(data.frame(
                        CACAonStrictaRMeanObs = CACAonStrictaRMeanObs_value,
                        CACAonStrictaRVarObs = 16.96768
                ),5)
        )
})


sims=5
n1_vec=c(5,10)
population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
#' avar = NULL
ovar = c(
        "Stricta",
        "CACA_on_Stricta"
)
avar = NULL
data(CactusRealizations)
popdata = CactusRealizations # WHY IS THERE ISLAND=NA
bad_dataframe <- data.frame(
        x=1, y=2
)

test_that("test sampleRealizations error handling, handleError_popdata", {
        expect_error(
                sampleRealizations(
                        popdata = bad_dataframe,
                        sims = sims,
                        n1_vec = n1_vec,
                        avar = avar,
                        ovar = ovar,
                        popvar="Island",
                        yvar="Cactus"
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
                        popvar="Island",
                        yvar="Cactus"
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
                        popvar="Island",
                        yvar=15
                ),
                "The argument 'yvar' must be a character string."
        )
})
test_that("test sampleRealizations errors, handleError_LogicalVar, SampleEstimators", {  
        expect_error(
                sampleRealizations(
                        popdata = popdata,
                        sims = sims,
                        n1_vec = 1,
                        avar = avar,
                        ovar = ovar,
                        popvar="Island",
                        yvar="Cactus",
                        SampleEstimators="fake"
                ),
                "The argument 'SampleEstimators' must be assigned a value of either TRUE or FALSE."
        )
})
test_that("test sampleRealizations errors, handleError_LogicalVar, SpatStat", {
        expect_error(
                sampleRealizations(
                        popdata = popdata,
                        sims = sims,
                        n1_vec = 1,
                        avar = avar,
                        ovar = ovar,
                        popvar="Island",
                        yvar="Cactus",
                        SpatStat="fake"
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
                        popvar="Island",
                        yvar="Cactus",
                        SamplingDesign="GRTS"
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
                        popvar="Island",
                        yvar="Cactus",
                        y_HT_formula="yHT"
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
                        popvar="Island",
                        yvar="Cactus",
                        var_formula="varyHT"
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
                        popvar="Island",
                        yvar="Cactus",
                        weights="We"
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
                        popvar="Island",
                        yvar="Cactus",
                        realvar=5
                ),
                "The argument realvar must be a character string."
        )
        
})
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