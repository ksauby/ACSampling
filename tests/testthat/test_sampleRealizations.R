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



# 
# 
# createSample <- function(SamplingDesign, popdata, seed, n1, yvar, f_max)
# 
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