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
