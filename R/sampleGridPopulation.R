#' Select a sample of cells from the grid population and randomly assign species information.
#' @param grid A dataframe of x and y coordinates (can be created with \code{createPopulation}).
#' @param n.networks Initial sample size that determines the relative density of the species.
#' @param cluster.centers Dataframe including x and y coordinates and network IDs for the centers of clusters.
#' @param seed Vector of numbers to be given to \code{set.seed()}. Two numbers are used: the first to determine the sample of locations and the second to determine the sample of species information to be assigned to the sample locations.
#' @description This function uses two random numbers: the first to set the seed to sample locations from a grid, and the second to sample networks to assign to those locations (more specifically, the centers of the networks are assigned to those locations).
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' 
#' @examples
# EXAMPLE 1
#' library(magrittr)
#' library(dplyr)
#' grid = createPopulation(0, 25, 0, 25)
#' n.networks = 2
#' data(Thompson1990Fig1Pop)
#' cluster.info = Thompson1990Fig1Pop %>% 
#' 	filter(m > 1) %>%
#' 	createNetworks
#' cluster.centers = filter(cluster.info, Rel_x==0 & Rel_y==0) %>%
#' dplyr::select(-c(x,y))
#' seed = 1:2
#' sampleGridPopulation(grid, n.networks, cluster.centers, seed)
#' @export


sampleGridPop <- function(grid, n.networks, cluster.centers, seed) {
     # determine locations
     set.seed(seed[1])
     gridsample <- grid[sample(x = 1:dim(grid)[1], size = n.networks),]
     gridsample$location.seed <- seed[1]
     # determine attributes of samples
     set.seed(seed[2])
     species <- cluster.centers[
          sample(x = 1:dim(cluster.centers)[1], size = n.networks),
     ] %>% as.data.frame
     gridsample$SpeciesInfo.seed <- seed[2]
     # merge location and attributes
     gridsample = cbind(gridsample, species)
     return(gridsample)
}	