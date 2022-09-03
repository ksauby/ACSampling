#' @importFrom tibble tibble

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
      createNetworkCenters(popdata) %>% as.data.frame,
      data.frame(
         NetworkID = unique(popdata$NetworkID),
         Center_x = popdata_Centerx,
         Center_y = popdata_Centery
      )
   )
   popdata2 <- data.frame(x = c(2, 2, 3, 3, 3, 3, 4, 4),
                          y = c(1, 2, 1, 2, 3, 4, 2, 4))
   expect_error(# because it's missing NetworkID
      createNetworkCenters(popdata2))
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
   expect_equal(createNetworks(popdata2),
                NetworksManual)
   expect_error(createNetworkCenters(popdata2))
   expect_equal(
      createNetworkCenters(popdata),
      tibble::tibble(
         NetworkID = c(1, 2),
         Center_x = c(3, 1),
         Center_y = c(2, 5)
      )
   )
})
test_that("sampleGridPop", {
   popdata2 <- data.frame(x = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                          y = c(1, 2, 3, 1, 2, 3, 1, 2, 3))
   seed <- 1
   # set.seed(seed)
   # sampleseeds <- runif(2)
   cluster.centers <- c(1, 2, 3, 4)
   n.networks = 2
   # set.seed(sampleseeds[1])
   # gridsample <- popdata2[sample(x = 1:dim(popdata2)[1], size = n.networks),]
   # # determine attributes of samples
   # set.seed(sampleseeds[2])
   # Networks <- cluster.centers[sample(x = 1:length(cluster.centers)[1], size = n.networks)]
   fakeGridPop <- data.frame(
      x = c(3, 2),
      y = c(3, 1),
      #seed = 1,
      NetworkID = c(2, 1)
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
   Rel_y = c(1, 0, -1, 1, 0, -1)#,
   #rotation.seed = 2008,
   #rotation = 90
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
      #rotation.seed = 5,
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
      #rotation.seed = 9,
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
      #rotation.seed = 2,
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
      #rotation.seed = 8,
      rotation = 270
   )
   
   expect_equal(
      ClusterExampleDataManualOutput,
      rotateCluster(ClusterExampleData, seed, Sx, Sy)
   )
})


test_that("randomizeClusters, seed=8, rotation=270 and 0", {
   seed <- 1
   grid <- createPop(5, 25, 5, 25)
   n.networks = 2
   cluster.info = ClusterExampleData2

   set.seed(seed)
   uniqueNetworkIDs <- unique(cluster.info$NetworkID)
   sampleseeds <- runif(length(unique(cluster.info$NetworkID)) + 1 + dim(grid)[1])
   # seed for rotation 1: 0.37212390; rotation = 90
   # set.seed(sampleseeds[2])
   # rotation1 = sample(c(0, 90, 180, 270), 1)
   # seed for rotation 2: 0.57285336; rotation = 90
   # set.seed(sampleseeds[3])
   # rotation2 = sample(c(0, 90, 180, 270), 1)


   # sampling of Networks
   set.seed(sampleseeds[1])
   moresampleseeds <- runif(2)
   # determine locations
   set.seed(moresampleseeds[1])
   gridsample <- grid[sample(x = 1:dim(grid)[1], size = n.networks),]
   # determine attributes of samples - Network 2 and Network 2
   set.seed(moresampleseeds[2])
   Networks <- data.frame(
      NetworkID = uniqueNetworkIDs[sample(x = 1:length(uniqueNetworkIDs)[1], size = n.networks)]
   )
   set.seed(sampleseeds[3])
   rotation = sample(c(0, 90, 180, 270), 1)
   set.seed(sampleseeds[4])
   rotation = sample(c(0, 90, 180, 270), 1)
   
   # n1plots = sampleGridPop(grid, n.networks, unique(cluster.info$NetworkID), sampleseeds[1])
   # 
   # set.seed(sampleseeds[1])
   # gridsample <- grid[sample(x = 1:dim(grid)[1], size = n.networks),]
   # # determine attributes of samples
   # set.seed(sampleseeds[2])
   # Networks <- data.frame(
   #    NetworkID = NetworkIDs[sample(x = 1:length(NetworkIDs)[1], size = n.networks)]
   # )
   # gridsample$seed <- seed
   # 
   # what happens when there is no buffer

   ClusterExampleData2ManualOutput <- data.frame(
      x = c(
         12,13,14,
         12,13,14,
         23,23,
         24,24,24,24,24,
         25,25,25
      ),
      y = c(
         19,19,19,
         20,20,20,
         22,24,
         21,22,23,24,25,
         21,23,24
      ),
      y_value = c(
         2,11,2,
         5,13,3,
         1,1,
         1,2,3,2,1,
         1,1,1
      ),
      #Center_x = ClusterExampleData2$Center_x,
      #Center_y = ClusterExampleData2$Center_y,
      #Rel_x = ClusterExampleData2$Rel_x,
      #Rel_y = ClusterExampleData2$Rel_y,
      #seed = 1,
      rotation = 90,
      #loc.selection.seed = 1,
      #network.selection.seed = 2,
      NetworkID = c(rep(1,6),rep(2,10)),
      m = c(rep(6,6),rep(10,10))
   )

   expect_equal(
      ClusterExampleData2ManualOutput %>%
         arrange(x,y),
      randomizeClusters(grid, n.networks, cluster.info, seed) %>%
         arrange(x,y)
   )
})
test_that("randomizeClusters, one network out of two", {
   seed=5
   set.seed(seed)
   cluster.info = ClusterExampleData2
   
   uniqueNetworkIDs <- unique(cluster.info$NetworkID)
   sampleseeds <- runif(length(unique(cluster.info$NetworkID)) + 1 + dim(grid)[1])
   # seed for rotation 1: 0.37212390; rotation = 90
   # set.seed(sampleseeds[2])
   # rotation1 = sample(c(0, 90, 180, 270), 1)
   # seed for rotation 2: 0.57285336; rotation = 90
   # set.seed(sampleseeds[3])
   # rotation2 = sample(c(0, 90, 180, 270), 1)
   
   
   # sampling of Networks
   set.seed(sampleseeds[1])
   moresampleseeds <- runif(2)
   # determine locations
   set.seed(moresampleseeds[1])
   gridsample <- grid[sample(x = 1:dim(grid)[1], size = n.networks),]
   # determine attributes of samples - Network 2 and Network 2
   set.seed(moresampleseeds[2])
   Networks <- data.frame(
      NetworkID = uniqueNetworkIDs[sample(x = 1:length(uniqueNetworkIDs)[1], size = n.networks)]
   )
   set.seed(sampleseeds[3])
   rotation = sample(c(0, 90, 180, 270), 1)
   set.seed(sampleseeds[4])
   rotation = sample(c(0, 90, 180, 270), 1)
   
   
   
   
   
   
   ClusterExampleData2ManualOutput <- data.frame(
      x = c(
         12,11,10, 12,11,10, 8, 9,9,9, 10,10, 11, 12,12
      ),
      y = c(
         21,21,21, 20,20,20, 22, 21,22,23, 22,23, 22, 22,23
      ),
      #m = c(6,10,6,6,6,6,10,10,10,10,10,10,10,10,10),
      y_value = c(2,11,2,5,13,3,1,1,2,1,3,1,2,1,1),
      #Center_x = c(11,23,11,11,11,11, rep(11,9)),
      #Center_y = ClusterExampleData2$Center_y,
      #Rel_x = ClusterExampleData2$Rel_x,
      #Rel_y = ClusterExampleData2$Rel_y,
      #rotation.seed = c(rep(8,6), rep(2,9)),
      rotation = c(rep(270,6), rep(0,9)),
      #loc.selection.seed = 5,
      #network.selection.seed = 2,
      NetworkID = 1,
      m=15
   )
   seed <- 5
   #seed = c(5, 2, # picks locations, then assigns NetworkIDs to locations
   #         8, 2, # rotation for each of the two Networks
   #         2, 3, 3, 4, 5)
   grid <- createPop(5, 25, 5, 25)
   n.networks = 2
   cluster.info = ClusterExampleData2

   expect_equal(
      ClusterExampleData2ManualOutput %>%
         arrange(x,y),
      randomizeClusters(grid, n.networks, cluster.info, seed, yvar="y_value") %>%
         arrange(x,y)
   )
})



