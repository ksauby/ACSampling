#' Create a Restricted Adaptive Cluster Sample, for any step size
#' 
#' @param popdata grid of population to be sampled.
#' @param seed vector of numbers to feed to \code{set.seed()} so that the sampling is reproducible.
#' @param n1 initial sample size (sampled according to simple random sampling without replacement).
#' @param yvar Variable of interest, used to determine condition under which adaptive cluster sampling takes place. Must be numeric. ACSampling is triggered when the y_variable is greater than the condition.
#' @param condition Threshold value of the y variable that initiates Restricted ACS. Defaults to \code{0}. Must be numeric.
#' @param f_max WHAT IS IT
#' @param initsample List of x and y coordinates of the initial sample. Defaults to "NA" so that the initial sample is selected according to simple random sampling without replacement.
#' @return A restricted adaptive cluster sample.
#' @examples
#' library(ggplot2)
#' popdata = lambdap_5_tau_1
#' seed=3
#' n1=5
#' yvar = "y_value"
#' f_max = 3
#' Z = createRACS(
#' 	popdata = lambdap_5_tau_1, 
#' 	n1 = n1, 
#' 	yvar = yvar, 
#' 	seed = seed, 
#' 	f_max = f_max
#' )

#' ggplot() +
#' geom_point(
#' data=Z, 
#' aes(x,y, size=factor(y_value), shape=factor(y_value))
#' ) +
#' scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' geom_point(data=Z, aes(x,y), shape = 0, size=7) +
#' ggtitle("f_max = 1")

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} A Sampling Strategy Designed to Maximize the Efficiency of Data Collection of Food Web Relationships.

#' @export
#' @importFrom dplyr everything bind_rows

createRACS <- function(popdata, n1, yvar, condition=0, seed=NA, initsample=NULL, f_max=2) {

     handleError_popdata(popdata)
     handleError_n1(n1)
     handleError_yvar(yvar)
     handleError_seed(seed)
     handleError_condition(condition)
 

  
     if (is.numeric(f_max)==TRUE) {
          if (!isTRUE(f_max == floor(f_max))) {
               stop("The 'f_max' argument must be an integer value.")
          } else {
          stop("The 'f_max' argument must be an integer value.")
     }
 }

     
	y_value <- x <- y <- Sampling <- NetworkID <- m <- NULL
	# get primary sample
	if (is.data.frame(initsample)) {
		S = merge(popdata, initsample, all.y=TRUE) 	
		S$Sampling <- "Primary Sample"
		S$step <- 0
	} else {
		if (!is.na(seed)) {set.seed(seed)}
			S <- createSRS(popdata, n1)
			S$step <- 0
	}
	# filter out primary samples that satisfy the condition
	Networks <- S %>% 
		filter(!! sym(yvar) > condition)
	# if there are units that satisfy the condition, fill in cluster/edge units
	if (dim(Networks)[1] > 0) {
		#names(S)[names(S) == yvar] <- 'y_value'
		#names(popdata)[names(popdata) == yvar] <- 'y_value'
		# Lists to save data
		Y = list()
		Z = list()
		# step 1: get all neighbors of primary samples matching condition
		for (i in 1:dim(Networks)[1]) {
               L = Networks[i, ]
               Y[[i]] <- list()
               # STEP 1
               
               Y[[i]][[1]] <- data.frame()
               # northern neighbor of SRSWOR plot
               Y[[i]][[1]][1, "x"] = L$x
               Y[[i]][[1]][1, "y"] = L$y + 1
               # southern neighbor of SRSWOR plot
               Y[[i]][[1]][2, "x"] = L$x
               Y[[i]][[1]][2, "y"] = L$y - 1
               # eastern neighbor of SRSWOR plot
               Y[[i]][[1]][3, "x"] = L$x + 1
               Y[[i]][[1]][3, "y"] = L$y
               # western neighbor of SRSWOR plot
               Y[[i]][[1]][4, "x"] = L$x - 1
               Y[[i]][[1]][4, "y"] = L$y
               Y[[i]] <- do.call(bind_rows, Y[[i]])
		}
		Z[[1]] <- do.call(bind_rows, Y)
		# merge neighbors and primary samples matching condition
		Z[[1]]$step <- 1
		Z[[1]] -> B
		# steps 2 to f_max
		if (f_max > 1) {
          # get all neighbors of c(primary samples matching condition, 
	     #    neighbors) matching condition
               for (j in 2:f_max) {
                    last_step = j - 1
                    A <- B %>% filter(.data$step == last_step)
                    Z[[j]] <- list()
                    if (dim(A)[1] > 0) {
                         for (k in 1:dim(A)[1]) {
                              Z[[j]][[k]] <- data.frame()
                              kx = A$x[k]
                              ky = A$y[k]
                              # if plot has cacti, survey its neighbors
                              if (dim(popdata %>%
                                      filter(!!sym(yvar) > condition,
                                             x == kx,
                                             y == ky))[1] > 0) {
                                   # neighbor to north
                                   Z[[j]][[k]][1, "x"] = kx
                                   Z[[j]][[k]][1, "y"] = ky - 1
                                   # neighbor to south
                                   Z[[j]][[k]][2, "x"] = kx
                                   Z[[j]][[k]][2, "y"] = ky + 1
                                   # neighbor to east
                                   Z[[j]][[k]][3, "x"] = kx + 1
                                   Z[[j]][[k]][3, "y"] = ky
                                   # neighbor to west
                                   Z[[j]][[k]][4, "x"] = kx - 1
                                   Z[[j]][[k]][4, "y"] = ky
                              }
                              if (dim(Z[[j]][[k]])[1] > 0) {
                                   Z[[j]][[k]]$step <- j
                              }
                         }
                         B <- do.call(bind_rows, Z[[j]]) %>%
                              filter(!(is.na(.data$x))) %>%
                              bind_rows(B)
                         Z[[j]] <- do.call(bind_rows, Z[[j]])
				}
			}
			sample <- do.call(rbind.data.frame, Z)
			} else {
				sample <- do.call(rbind.data.frame, Z)
		}
		sample %<>%
               merge(popdata, by = c("x", "y")) %>%
               filter(!is.na(.data$x) & !is.na(.data$y)) %>% # remove NAs
               bind_rows(S) %>% # merge with SRSWOR plots
               arrange(.data$step)
          # remove duplicates
		no_duplicates <- sample[!duplicated(sample[, c("x", "y")]), ]
		# give plots satisfying condition NetworkIDs
		X = no_duplicates %>% 
			filter(!! sym(yvar) > condition) %>%
		  	assignNetworkMembership
		# give primary sample plots not satisfying condition NetworkIDs
		Y = no_duplicates %>% filter(
			!! sym(yvar) <= condition, 
			.data$Sampling=="SRSWOR" | 
			.data$Sampling=="SRSWR" | 
			.data$Sampling=="Primary Sample"
		)
        Y$NetworkID <- seq(
			from = (max(X$NetworkID) + 1), 
			to = (max(X$NetworkID) + dim(Y)[1]), 
			by = 1
		)
        # get list of cluster/edge plots not satifying condition
        Z = no_duplicates %>% filter(
             !! sym(yvar) <= condition, 
             is.na(.data$Sampling)
        )
		# if there are plots not satisfying the condition, make NetworkIDs and m values of Cluster plots not satifying condition "NA"
		if (dim(Z)[1] > 0) {
			Z$NetworkID <- NA
			Z$Sampling <- "Edge"
			Z$m <- 0			
			# merge back together		
			Z = bind_rows(X, Y, Z)	
		} else {
			# merge back together		
			Z = bind_rows(X, Y)			
		}
		if (dim(Z[which(is.na(Z$Sampling)), ])[1] > 0) {
			Z[which(is.na(Z$Sampling)), ]$Sampling <- "Cluster"
		}
		# rename filtering variable
		Z %<>% select(
		     .data$x, 
		     .data$y, 
		     .data$NetworkID, 
		     .data$m, 
		     !! sym(yvar), 
		     .data$Sampling, 
		     .data$step
		)
		#names(Z)[names(Z) == 'y_value'] <- yvar
		# add species attribute data
		
		Z %<>% 
			merge(popdata %>% 
                    select(-c(
                         .data$NetworkID,
                         .data$m
                    ))
               ) %>%
               select(
                    .data$x, 
                    .data$y, 
                    .data$NetworkID, 
                    .data$m, 
                    .data$y_value, 
                    .data$Sampling, 
                    everything()
			)
		# warning	
		if (dim(Z[duplicated(Z[, c("x", "y")]), ])[1] > 0) {
			warning("Duplicates remaining in RACS sample")
			stop()
		}	
  		return(Z)
		} else {
			# add species attribute data to sample
			S %<>% merge(popdata)
			return(S)
	}
}