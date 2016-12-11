#' Create a Restricted Adaptive Cluster Sample
#' 
#' @param population grid of population to be sampled.
#' @param seed vector of numbers to feed to \code{set.seed()} so that the sampling is reproducible.
#' @param n1 initial sample size (sampled according to simple random sampling without replacement).
#' @param y_variable Variable of interest, used to determine condition under which adaptive cluster sampling takes place.
#' @param condition Threshold value of the y variable that initiates Restricted ACS. Defaults to \code{0}.
#' @param initial_sample List of x and y coordinates of the initial sample. Defaults to "NA" so that the initial sample is selected according to simple random sampling without replacement.
#' @return A restricted adaptive cluster sample.
#' @examples
#' library(ggplot2)
#' Z = createRACS(
	population = patch_data_3, 
	seed=26, 
	n1=40, 
	y_variable = "Cactus"
)
#' ggplot() +
#' 	geom_point(data=Thompson1990Figure1Population, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Z, aes(x,y), shape=0, size=7)

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} A Sampling Strategy Designed to Maximize the Efficiency of Data Collection of Food Web Relationships.

#' @export
	
			
createRACS <- function(population, n1, y_variable, condition=0, seed=NA, initial_sample=NA) {
	y_value <- x <- y <- Sampling <- NetworkID <- m <- everything <- NULL
	if (is.data.frame(initial_sample)) {
		S = merge(population, initial_sample, all.y=TRUE) 	
		S$Sampling <- "Primary Sample"
	} else {
		if (!is.na(seed)) {set.seed(seed)}
		S <- createSRS(population, n1)
	}
	Networks <- S %>% 
		filter(eval(parse(text = paste("S$", y_variable, sep=""))) > condition)
	# if there are units that satisfy the condition, fill in cluster/edge units
	if (dim(Networks)[1] > 0) {
		names(S)[names(S) == y_variable] <- 'y_value'
		names(population)[names(population) == y_variable] <- 'y_value'
		# List to save data
		Z = list()
		# fill in edge units
	    for (i in 1:dim(Networks)[1]) {
  	    	
			# for each unit that satisfies the condition, add its neighbors
			# step 1: neighbors in four cardinal directions
			# step 2: neighbors of neighbors....
			# step 3: ....
			
			
			L = Networks[i, ]
    	    Z[[i]] <- list()
			Z[[i]][[1]] <- as.data.frame(matrix(NA,1,1))
    	    # northern neighbor of SRSWOR plot
    	    Z[[i]][[1]][2, "x"] = L$x
    	    Z[[i]][[1]][2, "y"] = L$y + 1
	      	# southern neighbor of SRSWOR plot
	      	Z[[i]][[1]][3, "x"] = L$x
	      	Z[[i]][[1]][3, "y"] = L$y - 1
	      	# eastern neighbor of SRSWOR plot
	      	Z[[i]][[1]][4, "x"] = L$x + 1
	      	Z[[i]][[1]][4, "y"] = L$y
	      	# western neighbor of SRSWOR plot
	      	Z[[i]][[1]][5, "x"] = L$x - 1
	      	Z[[i]][[1]][5, "y"] = L$y
			# steps 2 - max
			if (max > 1) {
				for (j in 2:max) {
					Z[[i]][[j]] <- as.data.frame(matrix(NA,1,1))
					Z[[i]][[j]][, "j"] <- j
					# northern neighbor of cluster plot
					# 		if plot has cacti, survey its neighbors
					if (dim(population %>% 
						filter(
		  					y_value > condition, 
		  			  		x==L$x,
		  			  		y==L$y + j
					))[1] > 0
					) {
					    # neighbor to north
					    Z[[i]][[j]][1, "x"] = Z[[i]][[1]][2, "x"]
					   	Z[[i]][[j]][1, "y"] = Z[[i]][[1]][2, "y"] + j
					    # neighbor to east
					    Z[[i]][[j]][2, "x"] = Z[[i]][[1]][2, "x"] + j
					    Z[[i]][[j]][2, "y"] = Z[[i]][[1]][2, "y"]
					    # neighbor to west
					    Z[[i]][[j]][3, "x"] = Z[[i]][[1]][2, "x"] - j
					    Z[[i]][[j]][3, "y"] = Z[[i]][[1]][2, "y"]
					}
			      	# southern neighbor of cluster plot
					# 		if plot has cacti, survey its neighbors
					if (dim(population %>% 
						filter(
		  					y_value > condition, 
							x==L$x,
							y==L$y - j
					))[1] > 0
					) {
						# neighbor to south
					    Z[[i]][[j]][4, "x"] = Z[[i]][[1]][3, "x"]
					    Z[[i]][[j]][4, "y"] = Z[[i]][[1]][3, "y"] - j
					   	# neighbor to east
					    Z[[i]][[j]][5, "x"] = Z[[i]][[1]][3, "x"] + j
					    Z[[i]][[j]][5, "y"] = Z[[i]][[1]][3, "y"]
					    # neighbor to west
					    Z[[i]][[j]][6, "x"] = Z[[i]][[1]][3, "x"] - j
					    Z[[i]][[j]][6, "y"] = Z[[i]][[1]][3, "y"]
					}
			      	# eastern neighbor of cluster plot
					# 		if plot has cacti, survey its neighbors
					if (dim(population %>% 
					  	filter(
		  					y_value > condition, 
					  	 	x==L$x + j,
					  	  	y==L$y
					))[1] > 0
					) {
			        	# neighbor to south
			        	Z[[i]][[j]][7, "x"] = Z[[i]][[1]][4, "x"]
			        	Z[[i]][[j]][7, "y"] = Z[[i]][[1]][4, "y"] - j
			        	# neighbor to north
			        	Z[[i]][[j]][8, "x"] = Z[[i]][[1]][4, "x"]
			        	Z[[i]][[j]][8, "y"] = Z[[i]][[1]][4, "y"] + j
			        	# neighbor to east
			        	Z[[i]][[j]][9, "x"] = Z[[i]][[1]][4, "x"] + j
			        	Z[[i]][[j]][9, "y"] = Z[[i]][[1]][4, "y"]
					}
			      	# western neighbor of SRSWOR plot
					# 		if plot has cacti, survey its neighbors
					if (dim(population %>% 
					  	filter(
		  					y_value > condition, 
					  	  	x==L$x - j,
					  	  	y==L$y
					))[1] > 0
					) {
			        	# neighbor to south
			        	Z[[i]][[j]][10, "x"] = Z[[i]][[1]][5, "x"]
			        	Z[[i]][[j]][10, "y"] = Z[[i]][[1]][5, "y"] - j
			        	# neighbor to north
			        	Z[[i]][[j]][11, "x"] = Z[[i]][[1]][5, "x"]
			        	Z[[i]][[j]][11, "y"] = Z[[i]][[1]][5, "y"] + j
			        	# neighbor to west
			        	Z[[i]][[j]][12, "x"] = Z[[i]][[1]][5, "x"] - j
			        	Z[[i]][[j]][12, "y"] = Z[[i]][[1]][5, "y"]
			      	}
				}
	      	# Z[[i]][, "ClusterID"] = L[, "SamplingNumber"]			
			}	
		    Z[[i]] <- do.call(rbind.fill, Z[[i]])
	    } 
	    sample <- do.call(rbind.data.frame, Z)
) # compress plot list to dataframe
	    sample = merge(sample, population, all.x=T, by=c("x", "y")) %>%
	    	filter(!is.na(x) & !is.na(y)) %>% # remove NAs
	    	rbind.fill(S) %>% # merge with SRSWOR plots
			arrange(Sampling)
	    # remove duplicates
		no_duplicates <- sample[!duplicated(sample[, c("x", "y")]), ]
		# give plots satisfying condition NetworkIDs
		X = no_duplicates %>% 
			filter(y_value > condition) %>%
		  	assignNetworkMembership
		# give primary sample plots not satisfying condition NetworkIDs
		Y = no_duplicates %>% filter(
				y_value == condition, 
				Sampling=="SRSWOR" | Sampling=="SRSWR" | Sampling=="Primary Sample"
		)
        Y$NetworkID <- seq(
			from = (max(X$NetworkID) + 1), 
			to = (max(X$NetworkID) + dim(Y)[1]), 
			by = 1
		)
		# get list of cluster/edge plots not satifying condition
		Z = no_duplicates %>% filter(
				y_value == condition, 
				is.na(Sampling)
		)
		# if there are plots not satisfying the condition, make NetworkIDs and m values of Cluster plots not satifying condition "NA"
		if (dim(Z)[1] > 0) {
			Z$NetworkID <- NA
			Z$Sampling <- "Edge"
			Z$m <- 0			
			# merge back together		
			Z = rbind.fill(X,Y,Z)	
		} else {
			# merge back together		
			Z = rbind.fill(X,Y)			
		}
		if (dim(Z[which(is.na(Z$Sampling)), ])[1] > 0) {
			Z[which(is.na(Z$Sampling)), ]$Sampling <- "Cluster"
		}
		# rename filtering variable
		Z %<>% select(x, y, NetworkID, m, y_value, Sampling)
		names(Z)[names(Z) == 'y_value'] <- y_variable
		# add species attribute data
		Z %<>% 
			merge(population %>% select(-NetworkID, -m)) %>%
			select(x, y, NetworkID, m, y_value, Sampling, everything())
		# warning	
		if (dim(Z[duplicated(Z[, c("x", "y")]), ])[1] > 0) {
			warning("Duplicates remaining in RACS sample")
			stop()
		}	
  		return(Z)
	} 
	else {
		# add species attribute data to sample
		S %<>% merge(population)
		return(S)
	}
}