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
               m=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          ),
          Stricta_yHT = new_y_HT(
               y=exampleCactusPop_filtered$Stricta, 
               N=N, 
               n1=n1, 
               m_threshold=2, 
               pi_i_values=NULL, 
               m=exampleCactusPop_filtered$m,
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
               m=exampleCactusPop_filtered$m,
               sampling=NULL, 
               criterion=NULL
          ),
          Stricta_yHT = y_HT(
               y=exampleCactusPop_filtered$Stricta, 
               N=N, 
               n1=n1, 
               pi_i_values=NULL, 
               m=exampleCactusPop_filtered$m,
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

test_that("test calc_y_HT_MultipleVars, var_y_HT_RACS", {
     
     exampleCactusPop_filtered <- exampleCactusPop %>%
          filter(!(is.na(NetworkID))) %>%
          group_by(NetworkID) %>%
          filter(row_number()==1) %>%
          ungroup()
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_var_yHT_RACS = var_y_HT_RACS(
               N=N, 
               n1=n1, 
               m=exampleCactusPop_filtered$m,
               y=exampleCactusPop_filtered$Cactus, 
               m_threshold=2
          ),
          Stricta_var_yHT_RACS = var_y_HT_RACS(
               N=N, 
               n1=n1, 
               m=exampleCactusPop_filtered$m,
               y=exampleCactusPop_filtered$Stricta, 
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
test_that("test calc_y_HT_MultipleVars, var_y_HT", {

exampleCactusPop_filtered <- exampleCactusPop %>%
     filter(!(is.na(NetworkID))) %>%
          group_by(NetworkID) %>%
          filter(row_number()==1) %>%
          ungroup()
     
     Cactus_and_Stricta_estimates <- data.frame(
          Cactus_var_yHT_RACS = var_y_HT(
               N=N, 
               n1=n1, 
               m=exampleCactusPop_filtered$m,
               y=exampleCactusPop_filtered$Cactus
          ),
          Stricta_var_yHT_RACS = var_y_HT(
               N=N, 
               n1=n1, 
               m=exampleCactusPop_filtered$m,
               y=exampleCactusPop_filtered$Stricta
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

