#' Assign network membership to adaptive cluster sample units.

#' @param popdata Dataset to be used, containing information about units containing the species of interest, including x and y coordinates, with column names "x" and "y," respectively.
#' @param plot.size The length and width of plots, in coordinate units. Defaults to 1.

#' @description This function assigns network membership to units in an adaptive cluster sample; if units are neighbors, they are assigned to the same network ID. 

#' @return Returns popdata with a new column for Network ID as well as a column for m, the number of units in the network.

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @examples
#' data(Thompson1990Figure1Population)
#' 
#' # plot species abundance
#' library(magrittr)
#' library(ggplot2)
#' library(dplyr)
#' ggplot(data=Thompson1990Figure1Population %>% 
#' 	filter(y_value > 0), aes(x,y, size=factor(y_value))) + geom_point()
#' 
#' # assign network membership of units containing the species of interest
#' P_networks <- assignNetworkMembership(Thompson1990Figure1Population %>%
#' 	 filter(y_value > 0))
#' 
#' # plot networks
#' ggplot(
#' 	data=P_networks, 
#' 	aes(x,y, size=factor(y_value), shape=factor(NetworkID))
#' ) + geom_point()
#' # coordinates should be given as x and y

#' @export
#' @importFrom dplyr group_by mutate summarise_each filter arrange select
#' @importFrom igraph clusters
#' @importFrom intergraph asIgraph
#' @importFrom magrittr %$% %>% %<>%
#' @importFrom network network
#' @importFrom stats dist

assignNetworkMembership <- function(popdata, plot.size=1) {
	NetworkID <- x <- NULL
	D <- as.matrix(dist(cbind(popdata$x, popdata$y), method="euclidian"))
	D = ifelse(D > plot.size, 0, D)
	D %<>% as.data.frame
	G <- network(D, directed=FALSE) %>% asIgraph()
	popdata$NetworkID <- clusters(G)$membership
	popdata %<>%
		group_by(NetworkID) %>%
		mutate(m = length(x)) %>%
		as.data.frame
	return(popdata)
}