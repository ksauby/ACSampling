#' Create patches of species of interest within the grid population.
#' @param n.networks Number of networks to sample from the population of networks.
#' @param Rel_x The distance of a plot from the center of the network to which it belongs on the x axis
#' @param Rel_y The distance of a plot from the center of the network to which it belongs on the y axis
#' @param grid A dataframe of x and y coordinates (can be created with \code{createPopulation}). Should only include plots that have the species of interest.
#' @param seed Vector of numbers to be given to \code{set.seed()}. Two numbers are used: the first to determine the sample of locations and the second to determine the sample of species information to be assigned to the sample locations.
#' @param cluster.info Dataframe of all network plots except for the center plot (which is included in the cluster.centers dataframe). The dataframe should, at a miminum, include fields with information about the species used to initiate ACS, a PlotID field, a NetworkID field, and x and y coordinates.
#' @param x X coodinate. Default is "x."
#' @param y Y Coordinate. Default is "y."
#' @description This function creates patches of the species of interest within the grid of locations created with \code{createPopulation}. First, it randomly determines the locations of network centers and randomly assigns species information to those locations (\code{sampleGridPopulation}), randomly rotates thet network orientation to further randomize the real data, then adds the rest of the neighbor plots. Finally, it assigns each plot a Network ID (using \code{assignNetworkMembership}; it is important to include this function in case networks overlap and must be given a new network ID).
#' 
#' If species information is assigned twice to a given unit (as can happen in the case of overlapping, neighboring networks), the unit will be preferentially assigned the information where the species is present (if only one of the duplicate records has the species as present). If there are more than one records indicating the species is present, one of the duplicate records is randomly drawn and then assigned to the unit.
#' 
#' The function returns a list, the first object being the list of units occupied by the species within the population, and the second object being a vector of the remaining, unused seed numbers.
#'
#' This function uses a maximum of \code{2 + n.networks} random numbers: the first to set the seed to sample locations from a grid, and the second to sample networks to assign to those locations (more specifically, the centers of the networks are assigned to those locations). Then a maximum of \code{n.networks} random numbers are used, each number to randomly rotate a network of units before it is assigned coordinates.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

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
#' grid <- createPopulation(
#' 	x_start + buffer, 
#' 	x_end - buffer, 
#' 	y_start + buffer, 
#' 	y_end - buffer
#' )
#' n.networks = 3
#' seed = 2000:2100
#' data(Thompson1990Fig1Pop)
#' cluster.info = Thompson1990Fig1Pop %>% 
#' 	dplyr::filter(m > 1) %>%
#' 	createNetworks
#' 
# create realization
#' Thompson1990_realization = createSpeciesPatch(grid, n.networks, seed, 
#' 	cluster.info, x=x, y=y, Rel_x=Rel_x, Rel_y=Rel_y)
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

createSpeciesPatch <- function(grid, n.networks, seed, cluster.info, x=x, y=y, Rel_x=Rel_x, Rel_y=Rel_y) {
	NetworkID <- temp_coords <- NULL
	cluster.centers <- cluster.info %>% 
	     filter(
	          .data$Rel_x==0, 
	          .data$Rel_y==0
	    )
	# determine locations and species information for stage 1 plots
	n1plots = sampleGridPop(grid, n.networks, cluster.centers, seed)
	seed = seed[-(1:2)]
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
		Clusters$rotation.seed <- seed[i]
    	set.seed(seed[i])
    	rotation = sample(c(0, 90, 180, 270), 1)
    	seed <- seed[-1]
    	Clusters$rotation <- rotation
    	# determine cluster plot coordinates	
		if (rotation==0){
			Clusters$x = Clusters$Rel_x + S$x
			Clusters$y = Clusters$Rel_y + S$y
		} else 
    	if (rotation==90) {
			Clusters$x = -Clusters$Rel_y + S$x
			Clusters$y = Clusters$Rel_x + S$y
      	} else
      	if (rotation==180) {
			Clusters$x = -Clusters$Rel_x + S$x
		    Clusters$y = -Clusters$Rel_y + S$y
      	} else
	  	if (rotation==270) {
			Clusters$x = Clusters$Rel_y + S$x
			Clusters$y = -Clusters$Rel_x + S$y
		}
		Z[[i]] <- Clusters
	}
  	H <- do.call(rbind.data.frame, Z)
  	# merge list of cluster plots with list of SRSWOR plots
  	H %<>%
		bind_rows(n1plots) %>%
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
          # if more than one row, randomly choose a row
            Y[i, ] = L[sample(1:dim(L)[1], size=1), ]
		}
    }
    names(Y) <- names(H)
   	Y %<>% 
   		select(-c(.data$NetworkID, .data$temp_coords)) %>%
		assignNetworkMembership(1)
	return(Y)
}