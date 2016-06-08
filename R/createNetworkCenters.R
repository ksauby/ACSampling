#' Create Network Centers
#' @param Species.Data dataframe containing Network IDs, x and y coordinates
#' @description Takes dataframe containing Network IDs, x and y coordinates, and returns the coordinates of the centers of the networks, in integer values.
#' coordinates should be labeled x and y and Network ID given as "NetworkID"

createNetworkCenters <- function(Species.Data) {
	NetworkID <- x <- y <- NULL
	Z = Species.Data %>%
		group_by(NetworkID) %>%
		summarise(
			Center_x = round(mean(x)),
			Center_y = round(mean(y))
		)
	return(Z)
}
