#' Table 1 data from Thompson (1990)
#'
#' Data from Table 1 from Thompson (1990). The table summarizes all possible outcomes when applying adaptive cluster sampling to a five-unit population with y-values equal to 1, 0, 2, 10, and 1000.
#'
#' \itemize{
#'   \item sampling_effort. Each possible outcome (numbered 1 through 10) of applying adaptive cluster sampling to the five-unit population.
#'   \item sampling. Whether the unit was sampled in the first stage by simple random sampling without replacement (SRSWOR) or whether it was sampled in the second stage by adaptive cluster sampling (ACS).
#'   \item y_value. The y-value of the sampled unit.
#'   \item m_k. The size (number of units) of the network, where the criterion is that y > 0.
#' }
#'
#' @format A data frame with 31 rows and 4 variables.
#' @name Thompson1990Table1data
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#'
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
NULL