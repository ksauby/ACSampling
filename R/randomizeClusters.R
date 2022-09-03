#' @noRd
rotateCluster <- function(Clusters, seed, x, y) {
     #Clusters$rotation.seed <- seed
     set.seed(seed)
     rotation = sample(c(0, 90, 180, 270), 1)
     # seed <- seed[-1]
     Clusters$rotation <- rotation
     # determine cluster plot coordinates	
     if (rotation==0){
          Clusters$x = -Clusters$Rel_x + x
          Clusters$y = -Clusters$Rel_y + y
     } else 
          if (rotation==90) {
               Clusters$x = -Clusters$Rel_y + x
               Clusters$y = Clusters$Rel_x + y
          } else
               if (rotation==180) {
                    Clusters$x = Clusters$Rel_x + x
                    Clusters$y = Clusters$Rel_y + y
               } else
                    if (rotation==270) {
                         Clusters$x = Clusters$Rel_y + x
                         Clusters$y = -Clusters$Rel_x + y
                    }
     return(Clusters)
}



#' Assign a sample of cluster centers (derived from real data) to a sample of grid locations
#' @param grid A dataframe of x and y coordinates (can be created with \code{createPop}).
#' @template n_networks
#' @param NetworkIDs Vector of NetworkIDs to randomly assign to the grid.
#' @param seed A number to set random seeds, if a goal is the ability to reproduce the random sampling.
#' @description This function samples locations from a grid, and separately samples NetworkIDs, which are then assigned to those locations.
#' @examples
# EXAMPLE 1
#' library(magrittr)
#' library(dplyr)
#' grid = createPop(0, 25, 0, 25)
#' n.networks = 2
#' data(Thompson1990Fig1Pop)
#' NetworkIDs = Thompson1990Fig1Pop %>% 
#' 	filter(m > 1) %$%
#' 	unique(NetworkID)
#' seed = 1
#' sampleGridPop(grid, n.networks, NetworkIDs, seed)
#' @export


sampleGridPop <- function(grid, n.networks, NetworkIDs, seed) {
   if (!is.na(seed)) {
      set.seed(seed)
      sampleseeds <- runif(2)
   } else {
      sampleseeds <- runif(2)
   }
   # determine locations
   set.seed(sampleseeds[1])
   gridsample <- grid[sample(x = 1:dim(grid)[1], size = n.networks),]
   # determine attributes of samples
   set.seed(sampleseeds[2])
   Networks <- data.frame(
      NetworkID = NetworkIDs[sample(x = 1:length(NetworkIDs), size = n.networks)]
   )
   #gridsample$seed <- seed
   # merge location and attributes
   gridsample = cbind(gridsample, Networks) %>%
      as.data.frame
   return(gridsample)
}	

#' Randomly place networks within a grid population.
#' @param n.networks Number of networks to sample from the population of networks.
#' @param grid A dataframe of all x and y coordinates (as integer values) within a rectangular or square grid of coordinates for which the $y$-value *satisfies* the ACS criterion. (The grid can be first created with \code{createPop}, and then rows can be removed that do not satisfy the ACS criterion). Should only include plots that have the species of interest.
#' @param seed Vector of numbers to be given to \code{set.seed()}. Two numbers are used: the first to determine the sample of locations and the second to determine the sample of species information to be assigned to the sample locations.
#' @param cluster.info Data.frame of all network units. The data.frame should, at a minimum, include fields with information about the species used to initiate ACS, a PlotID field, a NetworkID field, and x and y coordinates.
#' @template yvar
#' @description This function creates patches of the species of interest within the grid of locations created with \code{createPop} based on a dataset provided. First, it randomly determines the new locations of network centers and (\code{sampleGridPop}),, and then it randomly rotates the each network to further randomize the real data. Finally, it reassigns NetworkID (using \code{assignNetworkMembership} as previously distinct networks may have been located close enough that they have now merged to create a single network.
#' 
#' If species information is assigned twice to a given unit (as can happen in the case of overlapping, neighboring networks), the unit will be preferentially assigned the information where the species is present (if only one of the duplicate records has the species as present). If there are more than one records indicating the species is present, one of the duplicate records is randomly drawn and then assigned to the unit.
#' 
#' The function returns a list, the first object being the list of units occupied by the species within the population, and the second object being a vector of the remaining, unused seed numbers.
#'
#' This function uses a maximum of \code{2 + n.networks + number of units in the smallest network} random numbers: the first to set the seed to sample locations from a grid, the second to sample networks to assign to those locations (more specifically, the centers of the networks are assigned to those locations). Then \code{n.networks} random numbers to determine the degrees to which to rotate each network of units before it is assigned coordinates. Finally, the last \code{n.networks} random numbers are used, if necessary, to randomly determine which unit is used if units from multiple networks overlap.
#' 
#' MENTION BUFFER?
#' @references 
#' \insertRef{saubyadaptive}{ACSampling}
#' @export
#' @importFrom plyr rbind.fill

#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' # realization info
#' x_start = 1
#' x_end = 30
#' y_start = 1
#' y_end = 30
#' buffer = 5
#' grid <- createPop(
#' 	x_start + buffer, 
#' 	x_end - buffer, 
#' 	y_start + buffer, 
#' 	y_end - buffer
#' )
#' n.networks = 3
#' seed = 2000:2100 # WHY DID I MAKE THIS MANY SEEDS?
#' 
#' data(Thompson1990Fig1Pop)
#' cluster.info = Thompson1990Fig1Pop %>% 
#' 	dplyr::filter(m > 1) %>%
#' 	createNetworks
#' 
# create realization
#' Thompson1990_realization = randomizeClusters(grid, n.networks, cluster.info, seed, yvar="y_value")
#' 
#' # plot realization
#' library(ggplot2)
#' p <- ggplot(Thompson1990_realization, aes(x, y, colour=NetworkID, 
#' 	label=NetworkID))
#' p + annotate("rect", xmin=x_start, xmax=x_end, ymin=y_start, ymax=y_end, 
#' alpha=0, colour="grey") + 
#'  	geom_point(aes(size=factor(y_value))) + 
#'  	scale_colour_gradientn(colours = rainbow(7)) +
#'  	geom_text(aes(label=NetworkID), hjust=0, vjust=0)


randomizeClusters <- function(grid, n.networks, cluster.info, seed, yvar) {
   
   #handleError_seed(seed)
   NetworkID <- temp_coords <- NULL
   #NetworkIDs <- cluster.info %>% 
   #   filter(Rel_x==0,Rel_y==0
   #   )
   uniqueNetworkIDs <- unique(cluster.info$NetworkID)
   nseeds <- length(unique(uniqueNetworkIDs)) + 1 + dim(grid)[1]
   if (!is.na(seed)) {
      set.seed(seed)
      sampleseeds <- runif(nseeds)
   } else {
      sampleseeds <- runif(nseeds)
   }
   # determine locations and Network IDs for network centers
   n1plots = sampleGridPop(grid, n.networks, uniqueNetworkIDs, sampleseeds[1])

   sampleseeds <- sampleseeds[-1]
   # create list of cluster plots and their coordinates
   Z <- vector("list", length(unique(n1plots$NetworkID)))
   for (i in 1:length(unique(n1plots$NetworkID))) {
      # n1plot plot[i] at center of cluster
      S = n1plots[which(n1plots$NetworkID == unique(n1plots$NetworkID)[i]), ]
      # cluster neighbors
      Clusters = cluster.info[which(cluster.info$NetworkID == 
                                       unique(n1plots$NetworkID)[i]), ]
      # further randomize real data by rotating clusters
      # max number of unique rotation.seeds = n.networks
      Z[[i]] <- rotateCluster(Clusters, sampleseeds[1], S$x, S$y)
      sampleseeds <- sampleseeds[-1]
   }
   H <- do.call(rbind.data.frame, Z)
   # merge list of cluster plots with list of SRSWOR plots
   n1plots %<>% dplyr::select(-c(x,y))
   H %<>%
      merge(n1plots) %>%
      mutate(temp_coords = paste(x, y, sep="_")) 
   # remove duplicates
   Y <- as.data.frame(matrix(NA,1,dim(H)[2]))
   for (i in 1:length(unique(H$temp_coords))) {
      # pull information based on x, y combo
      L = H[which(H$temp_coords == unique(H$temp_coords)[i]), ]
      # if only one row of information
      if (dim(L)[1]==1) {
         Y[i, ] = L
      } else {
         # do any rows satisfy the sampling criterion?
         L %<>% filter(.data[[yvar]] > 0)
         if (dim(L)[1]>0) {
            if (dim(L)[1]>1) {
               # if multiple units satisfy sampling criterion, randomly choose one
               set.seed(sampleseeds[1])
               Y[i, ] = L[sample(1:dim(L)[1], size=1), ]
               sampleseeds <- sampleseeds[-1]
            } else
            {
               # if only one row satisfying criterion, pick that one
               Y[i, ] = L
            }
         } else {
            # otherwise randomly choose a row
            set.seed(sampleseeds[1])
            Y[i, ] = L[sample(1:dim(L)[1], size=1), ]
            sampleseeds <- sampleseeds[-1]
         }
      }
   }
   names(Y) <- names(H)
   Y %<>% 
      select(-c(
         .data$NetworkID, 
         .data$temp_coords,
         .data$Center_x,
         .data$Center_y,
         .data$Rel_x,
         .data$Rel_y,
         .data$m
      )) %>%
      assignNetworkMembership(plot.size=1)
   return(Y)
}