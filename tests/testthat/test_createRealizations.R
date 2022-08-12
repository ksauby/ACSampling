n.networks = c(1, 2, 3)
n.realizations = 1
SpeciesInfo <- Thompson1990Fig1Pop %>%
   filter(m > 1)
variables = "y_value"
buffer = 5
start.seed = 1


test_that("createRealizations", {
   n.networks = c(1, 2, 3)
   n.realizations = 1
   SpeciesInfo <- Thompson1990Fig1Pop %>%
      filter(m > 1)
   variables = "y_value"
   buffer = 5
   start.seed = 1
   expect_error(
      createRealizations(
         "A",
         5,
         1,
         5,
         buffer,
         n.networks,
         n.realizations,
         SpeciesInfo,
         start.seed,
         variables
      ),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createRealizations(
         1,
         "A",
         1,
         5,
         buffer,
         n.networks,
         n.realizations,
         SpeciesInfo,
         start.seed,
         variables
      ),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createRealizations(
         1,
         5,
         "A",
         5,
         buffer,
         n.networks,
         n.realizations,
         SpeciesInfo,
         start.seed,
         variables
      ),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createRealizations(
         1,
         5,
         1,
         "A",
         buffer,
         n.networks,
         n.realizations,
         SpeciesInfo,
         start.seed,
         variables
      ),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
})

test_that("createRealizations", {
   exampleCactusPop <- data.frame(
      Cactus =          c(1, 1, 1, 1, 1, 1),
      Stricta =         c(1, 1, 0, 0, 1, 0),
      CACAonStricta =   c(0, 1, 0, 0, 1, 0),
      x =               c(2, 3, 1, 2, 3, 2),
      y =               c(1, 1, 2, 2, 2, 3),
      NetworkID =       c(1, 1, 1, 1, 1, 1)
   )
   
   exampleCactusPop_output <- data.frame(
      Cactus =          c(0,  1, 1, 0,   1, 1, 1, 0,   0,  1, 0,  0,    0,  0,  0, 0),
      Stricta =         c(0,  1, 1, 0,   0, 0, 1, 0,   0,  0, 0,  0,    0,  0,  0, 0),
      CACAonStricta =   c(NA, 0, 1, NA,  0, 0, 1, NA,  NA, 0, NA, NA,   NA, NA, NA, NA),
      x =               c(1,  2, 3, 4,   1, 2, 3, 4,   1,  2, 3,  4,    1,  2,  3, 4),
      y =               c(1,  1, 1, 1,   2, 2, 2, 2,   3,  3, 3,  3,    4,  4,  4, 4),
      NetworkID =       c(2,  1, 1, 8,   1, 1, 1, 9,   3,  1, 6,  10,   4,  5,  7, 11)
   ) %>%
      dplyr::select(x, y, Cactus, Stricta, CACAonStricta, NetworkID) %>%
      arrange(x, y)
   
   createdbyfunction <- createAbsenceData(x_start=1, x_end=4, y_start=1, y_end=4,
      patch=exampleCactusPop, variables=c("Cactus","Stricta"))
   
   expect_equal(
      exampleCactusPop_output, 
      createAbsenceData(
         x_start=1, 
         x_end=4, 
         y_start=1, 
         y_end=4, 
         patch=exampleCactusPop, 
         variables=c("Cactus","Stricta")
      )
   )
})