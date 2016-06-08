#' Create patches of species of interest within the grid population. (formerly function_createHabitatPatch)
#' @param Species.Data A dataframe of x and y coordinates of locations and associated species information. Should only include plots that have the species of interest.
#' @description Takes dataframe containing Network IDs, x and y coordinates, and returns the coordinates of the centers of the networks, in integer values, as well as coordinates for each unit in a network, relative to the center of the network. If Network.Centers is supplied, it should be a dataframe, one row per network, giving the coordinates of the centers of the networks
#'  returns the dataframe with additional columns for the network center coordinates as well as coordinates for each individual relative to the center of the network
#' 
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples 
#' library(ggplot2)
#' library(magrittr)
#' library(dplyr)
#' # visualize Rel_x coordinates
#' ggplot(PlotSurveys_season1 %>% filter(Island==6), aes(x,y, 
#' colour=NetworkID, label=Rel_x)) + geom_point() + 
#' scale_colour_gradientn(colours = rainbow(7)) + geom_text(aes(label=Rel_x), 
#' hjust=0, vjust=0)
#' 
#' # visualize Rel_y coordinates
#' ggplot(PlotSurveys_season1 %>% filter(Island==6), aes(x,y, 
#' 	colour=NetworkID, label=Rel_y)) + geom_point() + 
#' scale_colour_gradientn(colours = rainbow(7)) + geom_text(aes(label=Rel_y), 
#' hjust=0, vjust=0)
#' 
#' # Example 2: UMBC Data
#' # filter data for ACSA3 in the KN plot
#' # UMBC_ACSA3 = UMBC_2014_census_file %>% filter(Plot=="KN" & spcode=="ACSA3")
#' 
#' # convert point patterns to grid of species occurrences/abundances
#' # Z <- UMBC_ACSA3 %>%
#' # 	mutate(
#' # 		floor_x = floor(x),
#' # 		floor_y = floor(y)
#' # 	)
#' # create grid of units
#' # pop = createPopulation(
#' # 	min(Z$floor_x), 
#' # 	max(Z$floor_x), 
#' # 	min(Z$floor_y),
#' # 	max(Z$floor_y))
#' # assign unit IDs to individual plants
#' # Z %<>% merge(pop, by.x=c("floor_x", "floor_y"), by.y=c("x", "y"))
#' # summarise plant information for each grid unit
#' # Z.summary = Z %>% group_by(unitID) %>%
#' # 	summarise(
#' # 		n.trees = length(spcode[which(spcode=="ACSA3")]),
#' # 		mean.DBH = mean(dbh, na.rm=T),
#' # 		x = floor_x[1],
#' # 		y = floor_y[1]
#' # 	)
#' # ACSA3.networks <- createNetworks(Z.summary)
#' 
#' # plot grid of ACSA3 abundance
#' # ggplot(ACSA3.networks, aes(x,y, size=as.factor(n.trees))) + geom_point()
#' 
#' # plot grid of ACSA3 Networks
#' # ggplot(ACSA3.networks, aes(x,y, colour=NetworkID, label=NetworkID)) + 
#' # geom_point() + scale_colour_gradientn(colours = rainbow(7)) + 
#' # geom_text(aes(label=NetworkID), hjust=0, vjust=0)
#'
#' # plot m values of Networks
#' # ggplot(ACSA3.networks, aes(x,y, colour=NetworkID, label=m)) + geom_point() + 
#' # scale_colour_gradientn(colours = rainbow(7)) + geom_text(aes(label=m), 
#' # hjust=0, vjust=0)
#'
#' # plot networks and label network centers
#' # ggplot(ACSA3.networks, aes(x,y, colour=NetworkID, label=m)) + geom_point() + 
#' # scale_colour_gradientn(colours = rainbow(7)) + 
#' # geom_text(aes(label=ifelse(Rel_x==0 & Rel_y==0, as.character(NetworkID), "")))
#' @export

createNetworks <- function(Species.Data) {
	NetworkID <- x <- Center_x <- y <- Center_y <- NULL
	Z = assignNetworkMembership(Species.Data, plot.size=1)
	centers = createNetworkCenters(Z)
	Species.Data = merge(Z, centers, by="NetworkID", all=T) %>%
		group_by(NetworkID) %>%
		mutate(
			Rel_x = x - Center_x,
			Rel_y = y - Center_y
		)
	return(Species.Data)
}