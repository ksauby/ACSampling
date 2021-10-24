test_that("createPop", {
     expect_error(
          createPop("A",5,1,5),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
     )
     expect_error(
          createPop(1, "A",1,5),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
     )
     expect_error(
          createPop(1,5,"A",5),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
     )
     expect_error(
          createPop(1,5,1,"A"),
          "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
     )
     expect_silent(
          createPop(1,5,1,5)
     )
     fakepop <- data.frame(
          x=c(1,2,3,1,2,3,1,2,3),
          y=c(1,1,1,2,2,2,3,3,3)
     )
     expect_equal(
          createPop(1,3,1,3),
          fakepop
     )
})
test_that("createNetworkCenters", {
     popdata <- data.frame(
          NetworkID = 1,
          x = c(2,2,3,3,3,3,4,4),
          y = c(1,2,1,2,3,4,2,4)
     )
     expect_equal(
          ACSampling:::createNetworkCenters(popdata) %>% as.data.frame,
          data.frame(NetworkID=1, Center_x=3, Center_y=2)
     )
     popdata2 <- data.frame(
          x = c(2,2,3,3,3,3,4,4),
          y = c(1,2,1,2,3,4,2,4)
     )
     expect_error(
          ACSampling:::createNetworkCenters(popdata2)
     )
})
