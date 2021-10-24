#' Create patches of species of interest within the grid population

#' @param popdata A dataframe of x and y coordinates of locations and associated species information. Should only include plots that have the species of interest.
#' @description Takes dataframe containing Network IDs, x and y coordinates, and returns the coordinates of the centers of the networks, in integer values, as well as coordinates for each unit in a network, relative to the center of the network. If Network.Centers is supplied, it should be a dataframe, one row per network, giving the coordinates of the centers of the networks
#'  returns the dataframe with additional columns for the network center coordinates as well as coordinates for each individual relative to the center of the network
#' 
#'  @noRd


createNetworks <- function(popdata) {
	NetworkID <- x <- Center_x <- y <- Center_y <- NULL
	Z = assignNetworkMembership(Species.Data, plot.size=1)
	centers = createNetworkCenters(Z)
	Species.Data = merge(Z, centers, by="NetworkID", all=T) %>%
		group_by(NetworkID) %>%
		mutate(
			Rel_x = x - Center_x,
			Rel_y = y - Center_y
		) %>%
	     ungroup() %>%
	     as.data.frame
	return(Species.Data)
}