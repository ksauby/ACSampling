#' Create Network Centers
#' @param popdata dataframe containing Network IDs, x and y coordinates
#' @description Takes dataframe containing Network IDs, x and y coordinates, and returns the coordinates of the centers of the networks, in integer values.
#' coordinates should be labeled x and y and Network ID given as "NetworkID"
#' @noRd

createNetworkCenters <- function(popdata) {
     
     handleError_popdata(popdata)
     NetworkID <- x <- y <- NULL
     Z = popdata %>%
          group_by(NetworkID) %>%
          summarise(
               Center_x = round(mean(x)),
               Center_y = round(mean(y))
          ) %>%
          ungroup()
     return(Z)
}


#' Create patches of species of interest within the grid population

#' @param popdata A dataframe of x and y coordinates of locations and information about the variable of interest $y$. This dataframe should only include plots that satisfy some criterion about the variable of interest $y$ (eg., value is greater than zero).
#' @description Takes dataframe containing Network IDs, x and y coordinates, and returns the coordinates of the centers of the networks, in integer values, as well as coordinates for each unit in a network, relative to the center of the network. If Network.Centers is supplied, it should be a dataframe, one row per network, giving the coordinates of the centers of the networks
#'  returns the dataframe with additional columns for the network center coordinates as well as coordinates for each individual unit relative to the center of the network
#' 
#'  @noRd


createNetworks <- function(popdata) {
	NetworkID <- x <- Center_x <- y <- Center_y <- NULL
	Z = assignNetworkMembership(popdata, plot.size=1)
	centers = createNetworkCenters(Z)
	popdata = merge(Z, centers, by="NetworkID", all=T) %>%
		group_by(NetworkID) %>%
		mutate(
			Rel_x = x - Center_x,
			Rel_y = y - Center_y
		) %>%
	     ungroup() %>%
	     as.data.frame
	return(popdata)
}