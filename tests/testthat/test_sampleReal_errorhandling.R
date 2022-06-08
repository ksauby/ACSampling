sims = 5
n1_vec = c(5, 10)
population <-
   createPop(
      x_start = 1,
      x_end = 30,
      y_start = 1,
      y_end = 30
   )
#' avar = NULL
ovar = c("Stricta",
         "CACA_on_Stricta")
avar = NULL
data(CactusRealizations)
popdata = CactusRealizations # WHY IS THERE ISLAND=NA
bad_dataframe <- data.frame(x = 1, y = 2)

test_that("test sampleRealizations error handling, handleError_popdata", {
   expect_error(
      sampleRealizations(
         popdata = bad_dataframe,
         sims = sims,
         n1_vec = n1_vec,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus"
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
         popvar = "Island",
         yvar = "Cactus"
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
         popvar = "Island",
         yvar = 15
      ),
      "The argument 'yvar' must be a character string."
   )
})
test_that("test sampleRealizations errors, handleError_LogicalVar, 
          SampleEstimators", {
             expect_error(
                sampleRealizations(
                   popdata = popdata,
                   sims = sims,
                   n1_vec = 1,
                   avar = avar,
                   ovar = ovar,
                   popvar = "Island",
                   yvar = "Cactus",
                   SampleEstimators = "fake"
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
         popvar = "Island",
         yvar = "Cactus",
         SpatStat = "fake"
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
         popvar = "Island",
         yvar = "Cactus",
         SamplingDesign = "GRTS"
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
         popvar = "Island",
         yvar = "Cactus",
         y_HT_formula = "yHT"
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
         popvar = "Island",
         yvar = "Cactus",
         var_formula = "varyHT"
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
         popvar = "Island",
         yvar = "Cactus",
         weights = "We"
      ),
      "weights must be supplied as 'W', 'B', 'C', 'U', or 'S'."
   )
   
})
test_that("test sampleRealizations errors, realvar", {
   expect_error(
      sampleRealizations(
         popdata = popdata,
         sims = sims,
         n1_vec = 1,
         avar = avar,
         ovar = ovar,
         popvar = "Island",
         yvar = "Cactus",
         realvar = 5
      ),
      "The argument realvar must be a character string."
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
