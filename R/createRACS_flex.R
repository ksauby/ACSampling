#' Create a Restricted Adaptive Cluster Sample
#' 
#' @template popdata
#' @template seed
#' @template n1
#' @template yvar
#' @template criterion
#' @template f_max
#' @template initsample
#' @return A restricted adaptive cluster sample.
#' @examples
#' @template samp_load_ggplot_Thompson1990fig
#' 
#' # Create RACS sample, seed=9
#' @template samp_Thompson1990fig_n110
#' f_max = 1
#' 
#' Sampling according to the RACS design
#' Z = createRACS(popdata, n1, yvar, seed, f_max)
#' 
#' In the plot, the open squares correspond to population units that were sampled
#' cluster sampling was not triggered because no units in the initial sampled
#' satisfied the criterion
#' ggplot() +
#'     geom_point(
#'         data=lambdap_10_tau_5, 
#'         es(x,y, size=y_value, shape=factor(y_value))
#'     ) +
#'     scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#'     geom_point(data=Z, aes(x,y), shape = 0, size=7) +
#'     ggtitle("f_max = 1")
#' 
#' Sampling according to the RACS design
#' seed = 7
#' Z = createRACS(popdata, n1, yvar, seed, f_max)
#' 
#' In the plot, the open squares correspond to population units that were sampled
#' cluster sampling was not triggered because no units in the initial sampled
#' satisfied the criterion
#' ggplot() +
#'     geom_point(
#'         data=lambdap_10_tau_5, 
#'         es(x,y, size=y_value, shape=factor(y_value))
#'     ) +
#'     scale_shape_manual(values=c(1, rep(16, length(2:42)))) +
#'     geom_point(data=Z, aes(x,y), shape = 0, size=7) +
#'     ggtitle("f_max = 3")
#' @references 
#' @template SaubyCitation

#' @export
#' @importFrom dplyr everything bind_rows

createRACS <- function(popdata, n1, yvar, criterion=0, seed=NA, initsample=NULL, f_max=2) {

     handleError_popdata(popdata)
     handleError_n1(n1)
     handleError_yvar(yvar)
     handleError_seed(seed)
     handleError_criterion(criterion)
 
     if (is.numeric(f_max)==TRUE) {
          if (f_max != floor(f_max)) {
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
	# filter out primary samples that satisfy the criterion
	Networks <- S %>% 
		filter(!! sym(yvar) > criterion)
	# if there are units that satisfy the criterion, fill in cluster/edge units
	if (dim(Networks)[1] > 0) {
		#names(S)[names(S) == yvar] <- 'y_value'
		#names(popdata)[names(popdata) == yvar] <- 'y_value'
		# Lists to save data
		Y = list()
		Z = list()
		# step 1: get all neighbors of primary samples matching criterion
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
		# merge neighbors and primary samples matching criterion
		Z[[1]]$step <- 1
		Z[[1]] -> B
		# steps 2 to f_max
		if (f_max > 1) {
          # get all neighbors of c(primary samples matching criterion, 
	     #    neighbors) matching criterion
               for (j in 2:f_max) {
                    last_step = j - 1
                    A <- B %>% filter(.data$step == last_step)
                    Z[[j]] <- list()
                    if (dim(A)[1] > 0) {
                         for (k in 1:dim(A)[1]) {
                              Z[[j]][[k]] <- data.frame()
                              kx = A$x[k]
                              ky = A$y[k]
                              # if criterion satisfied for a unit, survey its neighbors
                              if (dim(popdata %>%
                                      filter(!!sym(yvar) > criterion,
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
		# give plots satisfying criterion NetworkIDs
		X = no_duplicates %>% 
			filter(!! sym(yvar) > criterion) %>%
		  	assignNetworkMembership
		# give primary sample plots not satisfying criterion NetworkIDs
		Y = no_duplicates %>% filter(
			!! sym(yvar) <= criterion, 
			.data$Sampling=="SRSWOR" | 
			.data$Sampling=="SRSWR" | 
			.data$Sampling=="Primary Sample"
		)
        Y$NetworkID <- seq(
			from = (max(X$NetworkID) + 1), 
			to = (max(X$NetworkID) + dim(Y)[1]), 
			by = 1
		)
        # get list of cluster/edge plots not satifying criterion
        Z = no_duplicates %>% filter(
             !! sym(yvar) <= criterion, 
             is.na(.data$Sampling)
        )
		# if there are plots not satisfying the criterion, make NetworkIDs and m values of Cluster plots not satisfying criterion "NA"
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