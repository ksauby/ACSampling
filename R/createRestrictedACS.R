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
#' Z = createRestrictedACS(Thompson1990Figure1Population, seed=26, n1=10, "y_value")
#' ggplot() +
#' 	geom_point(data=Thompson1990Figure1Population, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Z, aes(x,y), shape=0, size=7)

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} A Sampling Strategy Designed to Maximize the Efficiency of Data Collection of Food Web Relationships.

#' @export
			
createRestrictedACS <- function(population, seed=1, n1, y_variable, condition=0, initial_sample=NA) {
	y_value <- x <- y <- Sampling <- NetworkID <- m <- everything <- NULL
	if (is.data.frame(initial_sample)) {
		S = merge(population, initial_sample, all.y=TRUE) 	
		S$Sampling <- "SRSWOR"
	} else {
		S <- createSRSWOR(population, seed, n1)
	}
	Networks <- filter(S, 
		eval(parse(text = paste("S$", y_variable, sep=""))) > condition)
	# if there are units that satisfy the condition, fill in cluster/edge units
	if (dim(Networks)[1] > 0) {
		names(S)[names(S) == y_variable] <- 'y_value'
		names(population)[names(population) == y_variable] <- 'y_value'
		# List to save data
		Z = list()
		# fill in edge units
	    for (i in 1:dim(Networks)[1]) {
  	    	L = Networks[i, ]
    	    Z[[i]] <- as.data.frame(matrix(NA,1,1))
    	    # northern neighbor of SRSWOR plot
    	    Z[[i]][2, "x"] = L$x
    	    Z[[i]][2, "y"] = L$y + 1
    	    # if plot has cacti, survey its neighbors
  	      	if (dim(population %>% 
				filter(
  					y_value > condition, 
  			  		x==L$x,
  			  		y==L$y + 1
				))[1] > 0
			) {
			    # neighbor to north
			    Z[[i]][6, "x"] = Z[[i]][2, "x"]
			   	Z[[i]][6, "y"] = Z[[i]][2, "y"] + 1
			    # neighbor to east
			    Z[[i]][7, "x"] = Z[[i]][2, "x"] + 1
			    Z[[i]][7, "y"] = Z[[i]][2, "y"]
			    # neighbor to west
			    Z[[i]][8, "x"] = Z[[i]][2, "x"] - 1
			    Z[[i]][8, "y"] = Z[[i]][2, "y"]
			}
	      	# southern neighbor of SRSWOR plot
	      	Z[[i]][3, "x"] = L$x
	      	Z[[i]][3, "y"] = L$y - 1
	      	# if plot has cacti, survey its neighbors
	      	if (dim(population %>% 
				filter(
  					y_value > condition, 
					x==L$x,
					y==L$y - 1
				))[1] > 0
			) {
				# neighbor to south
			    Z[[i]][9, "x"] = Z[[i]][3, "x"]
			    Z[[i]][9, "y"] = Z[[i]][3, "y"] - 1
			   	# neighbor to east
			    Z[[i]][10, "x"] = Z[[i]][3, "x"] + 1
			    Z[[i]][10, "y"] = Z[[i]][3, "y"]
			    # neighbor to west
			    Z[[i]][11, "x"] = Z[[i]][3, "x"] - 1
			    Z[[i]][11, "y"] = Z[[i]][3, "y"]
			}
	      	# eastern neighbor of SRSWOR plot
	      	Z[[i]][4, "x"] = L$x + 1
	      	Z[[i]][4, "y"] = L$y
	      	# if plot has cacti, survey its neighbors
	      	if (dim(population %>% 
			  	filter(
  					y_value > condition, 
			  	 	x==L$x + 1,
			  	  	y==L$y
				))[1] > 0
			) {
	        	# neighbor to south
	        	Z[[i]][12, "x"] = Z[[i]][4, "x"]
	        	Z[[i]][12, "y"] = Z[[i]][4, "y"] - 1
	        	# neighbor to north
	        	Z[[i]][13, "x"] = Z[[i]][4, "x"]
	        	Z[[i]][13, "y"] = Z[[i]][4, "y"] + 1
	        	# neighbor to east
	        	Z[[i]][14, "x"] = Z[[i]][4, "x"] + 1
	        	Z[[i]][14, "y"] = Z[[i]][4, "y"]
			}
	      	# western neighbor of SRSWOR plot
	      	Z[[i]][5, "x"] = L$x - 1
	      	Z[[i]][5, "y"] = L$y
	      	# if plot has cacti, survey its neighbors
	      	if (dim(population %>% 
			  	filter(
  					y_value > condition, 
			  	  	x==L$x - 1,
			  	  	y==L$y
				))[1] > 0
			) {
	        	# neighbor to south
	        	Z[[i]][15, "x"] = Z[[i]][5, "x"]
	        	Z[[i]][15, "y"] = Z[[i]][5, "y"] - 1
	        	# neighbor to north
	        	Z[[i]][16, "x"] = Z[[i]][5, "x"]
	        	Z[[i]][16, "y"] = Z[[i]][5, "y"] + 1
	        	# neighbor to west
	        	Z[[i]][17, "x"] = Z[[i]][5, "x"] - 1
	        	Z[[i]][17, "y"] = Z[[i]][5, "y"]
	      	}
	      	# Z[[i]][, "ClusterID"] = L[, "SamplingNumber"]				
	    } 
	    sample <- do.call(rbind.data.frame, Z) # compress plot list to dataframe
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
		# give SRSWOR plots not satisfying condition NetworkIDs
		Y = no_duplicates %>% filter(
				y_value == condition, 
				Sampling=="SRSWOR"
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