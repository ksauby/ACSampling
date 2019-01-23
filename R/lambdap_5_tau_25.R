#' Example dataset from Christman (1997)
#'
#' An example dataset from Christman (1997):\deqn{\lambda_P = 5, \tau = 2.5}{lambda_P = 5, tau = 2.5}.
#'
#' \itemize{
#'   \item y. Y coordinate.
#'   \item x. X coordinate.
#'   \item NetworkID. The ID of the network to which the unit belongs.
#'   \item m. The number of units in the given network.
#'   \item y_value. The abundance of the species of interest.
#' }
#'
#' @format A data frame with 100 rows and 5 variables
#' @name lambdap_5_tau_25
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.}  
#'
#' Christman, M. C. (1997). Efficiency of some sampling designs for spatially clustered populations. \emph{Environmetrics}, 8: 145--166.
#' @examples 
#' library(ggplot2)
#' library(magrittr)
#' data(lambdap_5_tau_25)
#' 
#' ggplot(lambdap_5_tau_25 %>% dplyr::filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))

NULL