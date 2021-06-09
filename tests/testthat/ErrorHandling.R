test_that("Error Handling - popdata", {
     popdata <- NA
     expect_error(
          handleError_popdata(popdata),
          "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'."
     )
     popdata <- 1
     expect_error(
          handleError_popdata(popdata),
          "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'."
     )
     popdata <- "random text"
     expect_error(
          handleError_popdata(popdata),
          "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'."
     )
})
test_that("Error Handling - seed", {
     seed <- 1.2
     expect_error(
          handleError_seed(seed),
          "The 'seed' argument must be a vector of integer values."
     )
     seed <- "random text"
     expect_error(
          handleError_seed(seed),
          "The 'seed' argument must be a vector of integer values."
     )
})
test_that("Error Handling - yvar", {
     yvar <- NA
     expect_error(
          handleError_yvar(yvar),
          "The argument 'yvar' must be a character string."
     )
     yvar <- 1
     expect_error(
          handleError_yvar(yvar),
          "The argument 'yvar' must be a character string."
     )
     yvar <- c("random text", "blah blah")
     expect_error(
          handleError_yvar(yvar),
          "The argument 'yvar' must be a character string."
     )
})
test_that("Error Handling - n1", {
     n1 <- NA
     expect_error(
          handleError_n1(n1),
          "The argument 'n1' must be an integer value."
     )
     n1 <- 1.2
     expect_error(
          handleError_n1(n1),
          "The argument 'n1' must be an integer value."
     )
     n1 <- "random text"
     expect_error(
          handleError_n1(n1),
          "The argument 'n1' must be an integer value."
     )
     n1 <- c(1,3,2)
     expect_error(
          handleError_n1(n1),
          "The argument 'n1' must be an integer value."
     )
})   
test_that("Error Handling - coord", {
     coord <- NA
     expect_error(
          handleError_coord(coord),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
     )
     coord <- 1.2
     expect_error(
          handleError_coord(coord),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
     )
     coord <- "random text"
     expect_error(
          handleError_coord(coord),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
     )
     coord <- c(1,3,2)
     expect_error(
          handleError_coord(coord),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
     )
})   




