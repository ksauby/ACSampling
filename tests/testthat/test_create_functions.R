test_that("createPop", {
   expect_error(
      createPop("A", 5, 1, 5),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createPop(1, "A", 1, 5),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createPop(1, 5, "A", 5),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_error(
      createPop(1, 5, 1, "A"),
      "A non-numeric value was passed to one of the coordinate arguments. Please provide a number."
   )
   expect_silent(createPop(1, 5, 1, 5))
   fakepop <- data.frame(x = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                         y = c(1, 1, 1, 2, 2, 2, 3, 3, 3))
   expect_equal(createPop(1, 3, 1, 3),
                fakepop)
})
test_that("createNetworkCenters", {
   popdata <- data.frame(
      NetworkID = 1,
      x = c(2, 2, 3, 3, 3, 3, 4, 4),
      y = c(1, 2, 1, 2, 3, 4, 2, 4)
   )
   popdata_Centerx <- floor(mean(popdata$x))
   popdata_Centery <- floor(mean(popdata$y))
   
   expect_equal(
      ACSampling:::createNetworkCenters(popdata) %>% as.data.frame,
      data.frame(
         NetworkID = popdata$NetworkID,
         Center_x = popdata_Centerx,
         Center_y = popdata_Centery
      )
   )
   popdata2 <- data.frame(x = c(2, 2, 3, 3, 3, 3, 4, 4),
                          y = c(1, 2, 1, 2, 3, 4, 2, 4))
   expect_error(# because it's missing NetworkID
      ACSampling:::createNetworkCenters(popdata2))
})
test_that("createNetworks", {
   popdata <- data.frame(
      NetworkID = c(rep(1, 8), 2),
      x = c(2, 2, 3, 3, 3, 3, 4, 4, 1),
      y = c(1, 2, 1, 2, 3, 4, 2, 4, 5)
   )
   popdata2 <- data.frame(x = c(2, 2, 3, 3, 3, 3, 4, 4, 1),
                          y = c(1, 2, 1, 2, 3, 4, 2, 4, 5))
   NetworksManual <- data.frame(
      NetworkID = c(rep(1, 8), 2),
      x = c(2, 2, 3, 3, 3, 3, 4, 4, 1),
      y = c(1, 2, 1, 2, 3, 4, 2, 4, 5),
      m = c(rep(8, 8), 1),
      Center_x = c(rep(3, 8), 1),
      Center_y = c(rep(2, 8), 5),
      Rel_x = c(-1, -1, 0, 0, 0, 0, 1, 1, 0),
      Rel_y = c(-1, 0, -1, 0, 1, 2, 0, 2, 0)
   )
   expect_equal(ACSampling:::createNetworks(popdata2),
                NetworksManual)
   expect_error(ACSampling:::createNetworkCenters(popdata2))
   expect_equal(
      ACSampling:::createNetworkCenters(popdata),
      tibble::tibble(
         NetworkID = c(1, 2),
         Center_x = c(3, 1),
         Center_y = c(2, 5)
      )
   )
})
test_that("createRealizations", {
   0
   
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
test_that("sampleGridPop", {
   popdata2 <- data.frame(x = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                          y = c(1, 2, 3, 1, 2, 3, 1, 2, 3))
   seed <- c(1, 5)
   cluster.centers <- c(1, 2, 3, 4)
   n.networks = 2
   fakeGridPop <- data.frame(
      x = c(3, 2),
      y = c(3, 1),
      loc.selection.seed = 1,
      network.selection.seed = 5,
      NetworkID = c(2, 3)
   )
   row.names(fakeGridPop) <- NULL
   fromFunction <-
      sampleGridPop(grid = popdata2, n.networks, cluster.centers, seed)
   row.names(fromFunction) <- NULL
   expect_equal(fromFunction,
                fakeGridPop)
})
ClusterExampleData <- data.frame(
   NetworkID = 1,
   x = c(rep(12, 3), rep(11, 3)),
   y = c(21, 22, 23, 21, 22, 23),
   m = 6,
   y_value = c(2, 11, 2, 5, 13, 3),
   Center_x = rep(11, 6),
   Center_y = rep(22, 6),
   Rel_x = c(-1, -1, -1, 0, 0, 0),
   Rel_y = c(1, 0, -1, 1, 0, -1),
   rotation.seed = 2008,
   rotation = 90
)
test_that("set up ClusterExampleData correctly", {
   expect_equal(
      rep(floor(mean(ClusterExampleData$x)), dim(ClusterExampleData)[1]),
      ClusterExampleData$Center_x
   )
   expect_equal(
      rep(floor(mean(ClusterExampleData$y)), dim(ClusterExampleData)[1]),
      ClusterExampleData$Center_y
   )
   expect_equal(
      (ClusterExampleData$Center_x - ClusterExampleData$x) == ClusterExampleData$Rel_x,
      rep(TRUE, dim(ClusterExampleData)[1])
   )
   expect_equal(
      (ClusterExampleData$Center_y - ClusterExampleData$y) == ClusterExampleData$Rel_y,
      rep(TRUE, dim(ClusterExampleData)[1])
   )
})
ClusterExampleData2 <- data.frame(
   NetworkID = c(rep(1, 6), rep(2, 10)),
   x = c(rep(12, 3), rep(11, 3),
         21, rep(22, 3), 23, 23, 24, 24, 25, 25),
   y = c(21, 22, 23, 21, 22, 23,
         11, 10, 11, 12, 11, 12, 10, 11, 11, 12),
   m = c(rep(6, 6), rep(10, 10)),
   y_value = c(2, 11, 2, 5, 13, 3,
               1, 1, 2, 1, 3, 1, 1, 2, 1, 1),
   Center_x = c(rep(11, 6), rep(23, 10)),
   Center_y = c(rep(22, 6), rep(11, 10)),
   Rel_x = c(-1, -1, -1, 0, 0, 0,
             2, 1, 1, 1, 0, 0, -1, -1, -2, -2),
   Rel_y = c(1, 0, -1, 1, 0, -1,
             0, 1, 0, -1, 0, -1, 1, 0, 0, -1)
)
test_that("set up ClusterExampleData2 correctly", {
   expect_equal(
      c(
         rep(
            floor(mean(ClusterExampleData2[which(ClusterExampleData2$NetworkID == 1),]$x)),
            6
         ),
         rep(
            floor(mean(ClusterExampleData2[which(ClusterExampleData2$NetworkID == 2),]$x)),
            10
         )
      ),
      ClusterExampleData2$Center_x
   )
   expect_equal(
      c(
         rep(
            floor(mean(ClusterExampleData2[which(ClusterExampleData2$NetworkID == 1),]$y)),
            6
         ),
         rep(
            floor(mean(ClusterExampleData2[which(ClusterExampleData2$NetworkID == 2),]$y)),
            10
         )
      ),
      ClusterExampleData2$Center_y
   )
   expect_equal(
      (ClusterExampleData2$Center_x - ClusterExampleData2$x) == ClusterExampleData2$Rel_x,
      rep(TRUE, dim(ClusterExampleData2)[1])
   )
   expect_equal(
      (ClusterExampleData2$Center_y - ClusterExampleData2$y) == ClusterExampleData2$Rel_y,
      rep(TRUE, dim(ClusterExampleData2)[1])
   )
})
   
   
   

Sx = 10
Sy = 20
test_that("rotateCluster, rotation=90", {
   seed = 5 # rotation will end up being 90
   ClusterExampleDataManualOutput <- data.frame(
      NetworkID = 1,
      x = c(9,10,11, 9,10,11),
      y = c(rep(19, 3), rep(20, 3)),
      m = 6,
      y_value = c(2, 11, 2, 5, 13, 3),
      Center_x = rep(11, 6),
      Center_y = rep(22, 6),
      Rel_x = c(-1, -1, -1, 0, 0, 0),
      Rel_y = c(1, 0, -1, 1, 0, -1),
      rotation.seed = 5,
      rotation = 90
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})
test_that("rotateCluster, rotation=180", {
   seed = 9 # rotation will end up being 180
   ClusterExampleDataManualOutput <- data.frame(
      NetworkID = 1,
      x = c(9,9,9, 10,10,10),
      y = c(21,20,19, 21,20,19),
      m = 6,
      y_value = c(2, 11, 2, 5, 13, 3),
      Center_x = rep(11, 6),
      Center_y = rep(22, 6),
      Rel_x = c(-1, -1, -1, 0, 0, 0),
      Rel_y = c(1, 0, -1, 1, 0, -1),
      rotation.seed = 9,
      rotation = 180
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})
test_that("rotateCluster, rotation=0", {
   seed = 2 # rotation will end up being 0
   ClusterExampleDataManualOutput <- data.frame(
      NetworkID = 1,
      x = c(11,11,11, 10,10,10),
      y = c(19,20,21, 19,20,21),
      m = 6,
      y_value = c(2, 11, 2, 5, 13, 3),
      Center_x = rep(11, 6),
      Center_y = rep(22, 6),
      Rel_x = c(-1, -1, -1, 0, 0, 0),
      Rel_y = c(1, 0, -1, 1, 0, -1),
      rotation.seed = 2,
      rotation = 0
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})
test_that("rotateCluster, rotation=270", {
   seed = 8 # rotation will end up being 270
   ClusterExampleDataManualOutput <- data.frame(
      NetworkID = 1,
      x = c(11,10,9, 11,10,9),
      y = c(21,21,21, 20,20,20),
      m = 6,
      y_value = c(2, 11, 2, 5, 13, 3),
      Center_x = rep(11, 6),
      Center_y = rep(22, 6),
      Rel_x = c(-1, -1, -1, 0, 0, 0),
      Rel_y = c(1, 0, -1, 1, 0, -1),
      rotation.seed = 8,
      rotation = 270
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})


test_that("randomizeClusters, seed=8, rotation=270 and 0", {
   seed = c(1, 2,
            8, 2
            1, 2, 3, 4, 5) # rotation will end up being 270
   grid <- createPop(5, 25, 5, 25)
   n.networks = length(unique(ClusterExampleData2$NetworkID))
   cluster.info = ClusterExampleData2
   x = x
   y = y
   Rel_x = Rel_x
   Rel_y = Rel_y
   
   
   # what happens when there is no buffer
   seed = c(8, 1, 2, 3, 4, 5) # rotation will end up being 270
   grid <- createPop(1, 30, 1, 30)
   n.networks
   seed
   cluster.info
   x = x
   y = y
   Rel_x = Rel_x
   Rel_y = Rel_y
   
   
   
   ClusterExampleDataManualOutput <- data.frame(
      NetworkID = 1,
      x = c(9, 9, 9, 10, 10, 10),
      y = c(21, 20, 19, 21, 20, 19),
      m = 6,
      y_value = c(2, 11, 2, 5, 13, 3),
      Center_x = 6,
      Center_y = 20,
      Rel_x = c(-1, 0, 1, -1, 0, 1),
      Rel_y = c(-1, -1, -1, 0, 0, 0),
      rotation.seed = 8,
      rotation = 270
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})


