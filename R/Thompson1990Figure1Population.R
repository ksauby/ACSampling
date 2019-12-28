#' Example dataset from Figure 1 from Thompson (1990)
#'
#' @description An example dataset from Figure 1 from Thompson (1990) containing the information on the y-values associated with units in a 20 by 20 unit grid.
#'
#' \itemize{
#'   \item x. X coordinate.
#'   \item y. Y coordinate.
#'   \item NetworkID. A unique number identifying the network to which the unit belongs.
#'   \item m. The number of units in the given network.
#'   \item y_value. The y-value associated with the unit.
#' }
#'
#' @format A data frame with 400 rows and 5 variables
#' @name Thompson1990Fig1Pop
#' @references Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @examples 
#' library(ggplot2)
#' 
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Fig1Sample)
#' 
#' # plot population
#' library(ggplot2)
#' ggplot(data=Thompson1990Fig1Pop, aes(x,y, size=factor(y_value), shape=factor(y_value))) +
#' 	geom_point() +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13))))
#'
#' # plot sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Thompson1990Fig1Sample, aes(x,y), shape=0, size=7)

NULL