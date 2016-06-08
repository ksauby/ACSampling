#' Assign network membership of adaptive cluster sample units.
#' @param dataframe Dataset to be used, containing information about units containing the species of interest, including x and y coordinates.
#' @param plot.size The length and width of plots, in coordinate units. Defaults to 1.
#' @description This function assigns network membership to units in an adaptive cluster sampling; if units are neighbors, they are assigned to the same network ID. 
#' @return Returns dataframe with a new column for Network ID as well as a column for m, the number of units in the network.
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.

#' @examples
#' # create population
#' empty = rep(0,20)
#' P = rbind(
#' 	c(rep(0,4),5,13,3,rep(0,13)),
#' 	c(rep(0,4),2,11,2,rep(0,13)),
#' 	as.data.frame(sapply(empty, rep, 11)), # rows 3-13
#' 	c(rep(0,9),3,1,rep(0,9)),
#' 	c(rep(0,8),5,39,10,rep(0,9)),
#' 	c(rep(0,8),5,13,4,rep(0,9)),
#' 	c(rep(0,7),2,22,3,rep(0,10)),
#' 	c(rep(0,12),10,8,rep(0,6)),
#' 	c(rep(0,12),7,22,rep(0,6)),
#' 	c(rep(0,20))
#' )
# add x and y coordinates
#' P = cbind(
#' 	expand.grid(x = rev(1:20), y = 1:20), 
#' 	y_value = as.vector(unlist(P))
#' 	)
#' names(P)[1:2] <- c("y", "x")
#' 
#' # plot species abundance
#' library(magrittr)
#' library(ggplot2)
#' library(dplyr)
#' ggplot(data=P %>% filter(y_value > 0), aes(x,y, size=factor(y_value))) + geom_point()
#' 
#' # assign network membership of units containing the species of interest
#' P_networks <- assignNetworkMembership(P %>% filter(y_value > 0))
#' 
#' # plot networks
#' ggplot(data=P_networks, aes(x,y, size=factor(y_value), shape=factor(NetworkID))) + geom_point()
#' # coordinates should be given as x and y

#' @export
#' @importFrom intergraph asIgraph
#' @importFrom stats dist
#' @importFrom network network
#' @importFrom igraph clusters
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise_each
#' @importFrom dplyr filter
#' @import data.table

assignNetworkMembership <- function(dataframe, plot.size=1) {
	NetworkID <- x <- NULL
	D <- as.matrix(dist(cbind(dataframe$x, dataframe$y), method="euclidian"))
	D = ifelse(D > plot.size, 0, D)
	D %<>% as.data.frame
	G <- asIgraph(network(D, directed=FALSE))
	dataframe$NetworkID <- clusters(G)$membership
	dataframe %<>%
		group_by(NetworkID) %>%
		mutate(m = length(x)) %>%
		as.data.frame
	return(dataframe)
}