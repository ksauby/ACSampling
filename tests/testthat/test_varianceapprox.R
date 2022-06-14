test_that("test yHTMultVarCalc, y_HT_RACS", {

   # Example from p. 138, 2006, Tille, Sampling Algorithims
   
   pi_i_values <- c(0.07, 0.17, 0.41, 0.61, 0.83, 0.91)
   N <- 6
   n=3
   
   Tille_2006_p138_results <- c(0.07812, 0.16932, 0.29028, 0.28548, 0.16932, 0.09828)
   
   expect_equal(
      Hajek_b(pi_i_values, N),
      Tille_2006_p138_results
   )
   
   # pi_i_values <- c(0.07, 0.17, 0.41, 0.61, 0.83, 0.91)
   # Tille_2006_p140_results <- c(0.06922,0.1643,0.3431,0.3335,0.1643,0.08866)
   # expect_equal(
   #    Hajek(pi_i_values, N),
   #    Tille_2006_p140_results
   # )
   
})
