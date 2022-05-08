#' Create multiple realizations from data

#' @template x_start
#' @template x_end
#' @template y_start
#' @template y_end
#' @param buffer The distance from the edge of the grid from which initial samples should not be taken.
#' @template n_networks
#' @param n.realizations The number of realizations to create per \code{n.networks}.
#' @param SpeciesInfo A dataframe of x and y coordinates and associated species information. Should only include plots that have the species of interest.
#' @param start.seed The initial number used in \code{set.seed}. All subsequent numbers used in set.seed will be incremental after this number. seed = seq(start.seed, start.seed + 2*n.realizations +  sum(n.networks)*n.realizations*2 + 1, by=1)
#' @param variables Dataframe column names that are included in the final patch realizations. These columsn are given a value of "0" if the species is not present or NA.
#' @description This function creates multiple realizations of patches of the species of interest within the grid of locations created with \code{createPopulation}.
#' @template SaubyCitation
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' # EXAMPLE 1
#' # how large does the buffer need to be when generating realizations? (so 
#' #	that network units are not truncated by the dimensions of the realization 
#' #	grid)
#' data(PlotSurveys_season1)
#' PlotSurveys_season1 %>%
#'	as.data.frame %>%
#' 	summarise(
#' 		max_x_buffer = max(abs(Rel_x)),
#' 		max_y_buffer = max(abs(Rel_y))
#' 	)
#' 
#' # realization info
#' x_start = 1
#' x_end = 30
#' y_start = 1
#' y_end = 30
#' n.networks = c(5, 15, 10, 20, 30, 40)
#' n.realizations = 1
#' SpeciesInfo = PlotSurveys_season1
#' variables = c("Stricta", "Pusilla", "Cactus")
#' start.seed=1
#' buffer=5
#' 
#' # create realizations
#' CactusRealizations = createRealizations(
#' x_start, 
#' x_end, 
#' y_start, 
#' y_end, 
#' buffer, 
#' n.networks, 
#' n.realizations, 
#' SpeciesInfo, 
#' start.seed,
#' variables
#' )
#' 
#' # plot realizations
#' p <- ggplot(CactusRealizations, aes(x, y, colour=NetworkID, 
#'		label=NetworkID))
#' p + annotate("rect", xmin=x_start, xmax=x_end, ymin=y_start, ymax=y_end,
#'		alpha=0, colour="grey") + 
#' 	geom_point(aes(size=factor(Cactus))) + 
#' 	facet_wrap(~n.networks) + 
#' 	scale_colour_gradientn(colours = rainbow(7)) +
#' 	geom_text(aes(label=NetworkID), hjust=0, vjust=0)
#'
#' p <- ggplot(CactusRealizations %>% filter(m>1), aes(x, y, colour=NetworkID,
#'	 	label=NetworkID))
#' p + annotate("rect", xmin=x_start, xmax=x_end, ymin=y_start, ymax=y_end,
#'		alpha=0, colour="grey") + 
#' 	geom_point(aes(size=factor(Cactus))) + 
#' 	facet_wrap(~n.networks) + 
#' 	scale_colour_gradientn(colours = rainbow(7)) +
#' 	geom_text(aes(label=NetworkID), hjust=0, vjust=0)
#'
#' # EXAMPLE 2
#' 
#' # realization info	
#' x_start = 1
#' x_end = 20
#' y_start = 1
#' y_end = 20
#' n.networks = c(1,2,3)
#' n.realizations = 1
#' SpeciesInfo = Thompson1990Fig1Pop %>% 
#' 	filter(m > 1) %>% 
#' 	createNetworks
#' variables = "y_value"
#' buffer=5
#' start.seed=1
#' 
#' # create realizations
#' Thompson.realizations = createRealizations(x_start, x_end,
#'	 	y_start, y_end, buffer, n.networks, n.realizations, SpeciesInfo, 
#'		start.seed, variables)	
#' 
#' # plot realizations
#' p <- ggplot(Thompson.realizations, aes(x, y, colour=NetworkID, label=NetworkID))
#' p + annotate("rect", xmin=x_start, xmax=x_end, ymin=y_start, ymax=y_end, 
#'		alpha=0, colour="grey") + 
#' 	geom_point(aes(size=factor(y_value))) + 
#' 	facet_wrap(~n.networks) + 
#' 	scale_colour_gradientn(colours = rainbow(7)) +
#' 	geom_text(aes(label=NetworkID), hjust=0, vjust=0)
#'
#' p <- ggplot(Thompson.realizations %>% filter(m > 1), aes(x, y, 
#'		colour=NetworkID, label=NetworkID))
#' p + annotate("rect", xmin=x_start, xmax=x_end, ymin=y_start, ymax=y_end,
#'		alpha=0, colour="grey") + 
#' 	geom_point(aes(size=factor(y_value))) + 
#' 	facet_wrap(~n.networks) + 
#' 	scale_colour_gradientn(colours = rainbow(7)) +
#' 	geom_text(aes(label=NetworkID), hjust=0, vjust=0)
#' @export

createRealizations <- function(
	x_start, 
	x_end, 
	y_start, 
	y_end, 
	buffer, 
	n.networks, 
	n.realizations, 
	SpeciesInfo, 
	start.seed, 
	variables
)
{
     
     handleError_coord(x_start)
     handleError_coord(x_end)
     handleError_coord(y_start)
     handleError_coord(y_end)
     
	x <- y <- Rel_x <- Rel_y <- NetworkID <- m <- NULL
	network.length = length(n.networks)
	seed = seq(start.seed, start.seed + 2*n.realizations + 
		sum(n.networks)*n.realizations*2 + 1, by=1)
    grid <- createPop(
		x_start + buffer, 
		x_end - buffer, 
		y_start + buffer, 
		y_end - buffer
	)
	patch.array <- vector("list", network.length)
	for (i in 1:length(n.networks)) { 	
		patch.array[[i]] <- list()
		for (j in 1:n.realizations) { 			
			patch = randomizeClusters(
				grid, 
				n.networks[i], 
				cluster.info=SpeciesInfo, 
				seed
			)	
			# fill in absence data
			population = createPop(
				x_start, 
				x_end, 
				y_start, 
				y_end
			)
			patch %<>% merge(population, by=c("x", "y"), all=T)
			for (k in 1:length(variables)) {
				patch[which(is.na(patch[,variables[k]])), 
					variables[k]] <- 0
			}
			patch[which(is.na(patch$NetworkID)), ]$NetworkID <- seq(
				max(patch$NetworkID, na.rm=T) + 1,
				length(patch[which(is.na(patch$NetworkID)), ]$NetworkID) + 
					max(patch$NetworkID, na.rm=T), 
				by=1
			) 
			# other info
			patch.array[[i]][[j]] <- patch
			patch.array[[i]][[j]]$n.networks <- n.networks[i]
			patch.array[[i]][[j]]$realization <- j
			# fill in missing m values
			patch.array[[i]][[j]] %<>% 
				group_by(NetworkID) %>% 
				mutate(m = length(m)) %>%
			     ungroup()
		}
		max.seed = max(patch$rotation.seed, na.rm=T)
		if (is.na(n.networks[i+1])) {seed = NA} else {
			seed = seq(max.seed, max.seed + 2*n.realizations + 
				sum(n.networks)*n.realizations*2 + 1, by=1)
		}
	}	
	# compress list of patch data to dataframe
	patch_data = do.call(bind_rows, unlist(patch.array, recursive=F))
	patch_data$N = dim(population)[1]
	return(patch_data)
}