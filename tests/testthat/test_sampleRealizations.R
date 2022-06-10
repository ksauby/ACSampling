sims = 1
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


test_that("test sampleRealizations", {
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



#' # 

sims=2
n1_vec=c(5,10)
population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
avar = NULL
ovar = c(
   "Stricta",
   "Pusilla",
   "Cactus"
)
yvar="Cactus"
rvar=NULL
SamplingDesign="ACS"
y_HT_formula = "y_HT"
var_formula = "var_y_HT"
mThreshold = NULL
f_max = 2
SampleEstimators = FALSE
SpatStat = TRUE
mChar = TRUE
popvar = "n.networks"
realvar = "realization"
weights="S"
seeds = 1:1000
data(CactusRealizations)
popdata = CactusRealizations # WHY IS THERE ISLAND=NA
simulation_data <- sampleRealizations(
   popdata,
   sims,
   n1_vec,
   avar,
   ovar,
   rvar,
   #ACS=TRUE,
   SamplingDesign,
   yvar,
   y_HT_formula,
   var_formula,
   mThreshold,
   f_max,
   SampleEstimators,
   SpatStat,
   mChar,
   popvar,
   realvar,
   weights,
   seeds
)



#' sims=200
#' n1_vec=c(75,150,225,300,350)
#' simulation_data_SRSWOR <- sampleRealizations(
#'    popdata = popdata,
#'    sims = sims,
#'    n1_vec = n1_vec,
#'    avar = avar,
#'    ovar = ovar,
#'    popvar="Island"
#' )
#' 
#' test_that("test sampleRealizations", {
#'    expect_error(
#'       sampleRealizations(
#'          popdata = popdata,
#'          sims = sims,
#'          n1_vec = 1,
#'          avar = avar,
#'          ovar = ovar,
#'          popvar = "Island",
#'          yvar = "Cactus",
#'          realvar = 5
#'       ),
#'       "The argument realvar must be a character string."
#'    )
#' 
#' })
# # data(Thompson1990Fig1Pop)
# # alldata_all <- createACS(Thompson1990Fig1Pop, 20, "y_value", seed=24)
# #
# # temp <- alldata_all %>%
# #         as.data.frame %>%
# #         # get rid of edge units - not involved in calculation of m
# #         filter(!(is.na(NetworkID))) %>%
# #         arrange(x, y)
# #
# # # dnearneigh - why was this here?
# #
# # nb <- cell2nb(
# #         nrow = max(temp$x) - min(temp$x) + 1,
# #         ncol = max(temp$y) - min(temp$y) + 1
# # )
# # coordinates(temp) = ~ x+y
# # data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
# # tempdat <- data.frame(JoinCountTest.W = NA)
# # for (i in length(weights)) {
# #         lwb <- nb2listw(nb, style = weights[i]) # convert to weights
# #         # I think cells are indexed by row, then column
# #         tempdat$JoinCountTest <- getJoinCountTestEst(temp, lwb)
# #         tempdat$MoranI <- getMoranTestEst(temp, lwb)
# #         colnames(tempdat)[which(names(tempdat) == "JoinCountTest")] <-
# #                 paste("JoinCountTest", weights[i], sep=".")
# #         colnames(tempdat)[which(names(tempdat) == "MoranI")] <-
# #                 paste("MoranI", weights[i], sep=".")
# # }
# # return(tempdat)