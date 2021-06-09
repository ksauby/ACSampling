handleError_popdata <- function(popdata) {
     if (is.data.frame(popdata)==FALSE) {
          stop(
               "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'.",
               call.=FALSE
          )
     } else if (all(c("x", "y", "NetworkID") %in% names(popdata)) == FALSE) {
          stop(
               "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'.",
               call.=FALSE
          )
     }
}
handleError_seed <- function(seed) {
     if (all(is.numeric(seed))==TRUE) {
          if (!isTRUE(all(seed == floor(seed)))) {
               stop(
                    "The 'seed' argument must be a vector of integer values.",
                    call.=FALSE
                    )
          }
     } else if (!is.na(seed)) {
          stop(
               "The 'seed' argument must be a vector of integer values.",
               call.=FALSE
               )
     }
}
handleError_yvar <- function(yvar) {
     if (is.character(yvar)==FALSE) {
          stop(
               "The argument 'yvar' must be a character string.",
               call.=FALSE
          )
     } else if (is.vector(yvar)==TRUE) {
          stop(
               "The argument 'yvar' must be a character string.",
               call.=FALSE
          )    
     }
}
handleError_n1 <- function(n1) {
     if (is.numeric(n1)==TRUE) {
          if (is.vector(n1)==TRUE) {
               stop(
                    "The argument 'n1' must be an integer value.",
                    call.=FALSE
               )
          } else if (round(n1)!=n1) {
               stop(
                    "The argument 'n1' must be an integer value.",
                    call.=FALSE
               )
          }
     } else {
          stop(
               "The argument 'n1' must be an integer value.",
               call.=FALSE
          )
     }
}
handleError_coord <- function(coord) {
     if (is.numeric(coord)==FALSE) {
          stop(
               "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
               call.=FALSE
          )
     } else if (is.vector(coord)==TRUE) {
          stop(
               "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
               call.=FALSE
          )
     }
}  
     
     
     

#' Create an Adaptive Cluster Sample.
#'
#' @param popdata A data frame providing information about the population to be sampled, including x and y columns, as well as a column containing the variable of interest, with the same name as that provided in the \code{yvar} argument.
#' @param seed A vector of integers to feed to \code{set.seed()} so that the sampling is reproducible. Defaults to NA so that it is not necessary to specify a random number seed.
#' @param n1 An integer giving the initial sample size (sampled according to simple random sampling without replacement).
#' @param yvar The variable of interest that is used to determine the condition under which adaptive cluster sampling takes place.
#' @param condition Threshold value of the y variable that initiates ACS. Defaults to 0 (i.e., anything greater than 0 initiates adaptive cluster sampling).
#' @param initsample Allows the user to specify a list of x and y coordinates of the initial sample. Defaults to "NA" so that the initial sample is selected according to simple random sampling without replacement. HOW IS THIS FORMATTED

#' @return A restricted adaptive cluster sample.

#' @examples
#' library(ggplot2)
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Figure1Sample)
#' 
#' # Initiate ACS
#' Z = createACS(
#'	popdata=Thompson1990Fig1Pop, 
#'	seed=9, 
#'	n1=10, 
#'	yvar="y_value", 
#'	condition=0
#')
#' 
#' # plot ACS sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Z, aes(x,y), shape=0, size=7)
#' # Initiate ACS, different seed
#' Z = createACS(popdata=Thompson1990Fig1Pop, seed=26, n1=10, yvar="y_value", condition=0)
#' 
#' # plot ACS sample overlaid onto population
#' ggplot() +
#' 	geom_point(data=Thompson1990Fig1Pop, aes(x,y, size=factor(y_value),
#' 		shape=factor(y_value))) +
#' 	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' 	geom_point(data=Z, aes(x,y), shape=0, size=7)

#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} A Sampling Strategy Designed to Maximize the Efficiency of Data Collection of Food Web Relationships.

#' @export
#' @importFrom stringr str_pad
#' @importFrom dplyr filter rowwise
#' @importFrom ggplot2 ggplot

createACS <- function(popdata, n1, yvar, condition=0, seed=NA, initsample=NA) {

     handleError_popdata(popdata)
     handleError_n1(n1)
     handleError_yvar(yvar)
     handleError_seed(seed)
     if (is.numeric(condition)==FALSE) {
          stop("The argument 'condition' must be numeric.")
     }
     
	YVAR <- sym(yvar)
	. <- Sampling <- y_val <- NULL
	if (is.data.frame(initsample)) {
		S <- merge(popdata, initsample, all.y=TRUE) 	
		S$Sampling <- "Primary Sample"
	} else {
		if (!is.na(seed)) {set.seed(seed)}
		S <- createSRS(popdata=popdata, n1=n1)
	}
	# add the rest of the units for each network in the initial sample
	Z = popdata %>%
		dplyr::filter(.data$NetworkID %in% S$NetworkID) %>%
		merge(S, all.x=T)
	Networks = Z %>% filter(!!YVAR > condition)
	# if there are units that satisfy the condition, fill in edge units
	if (dim(Networks)[1] > 0) {
		if (dim(Z[which(is.na(Z$Sampling)), ])[1] > 0) {
			Z[which(is.na(Z$Sampling)), ]$Sampling <- "Cluster"
		}
		# fill in edge units
		E = data.frame(
			x = as.numeric(rowSums(expand.grid(Networks$x, c(1,-1,0,0)))),
		  	y = rowSums(expand.grid(Networks$y, c(0,0,1,-1))),
			Sampling = "Edge"
		) %>%
		rowwise() %>%
		mutate(xy = paste(
			str_pad(
				.data$x,
				nchar(max(popdata$x)),
				"0",
				side="left"
			),
			str_pad(
				.data$y,
				nchar(max(popdata$y)),
				"0",
				side="left"
			)
		)) %>%
		ungroup()
		Z %<>% 
			mutate(xy = paste(
				str_pad(
					.data$x,
					nchar(max(popdata$x)),
					"0",
					side="left"
				),
				str_pad(
					.data$y,
					nchar(max(popdata$y)),
					"0",
					side="left"
				)
			)) %>%
			ungroup()
		E$Sampling %<>% as.character()
		E %<>% filter(!(.data$xy %in% Z$xy))
		ZZ <- Z %>% 
			bind_rows(E) %>%
			group_by(x,y) %>%
			filter(row_number()==1)
		# remove plots outside of population extent
		ZZ %<>% subset(
			x %in% popdata$x &
			y %in% popdata$y
		)
		# fill in values for Edge units
		if (dim(
			ZZ[which(
				is.na(
					eval(parse(text=paste(
						"ZZ$", 
						yvar, 
						sep=""
					)))
				)
			), ])[1] > 0) {
			ZZ %<>%
			rowwise() %>%
			mutate(
				!!YVAR := ifelse(
					Sampling=="Edge",
					0,
					!!YVAR
				),
				m = ifelse(
					Sampling=="Edge",
					0,
					m
				)
			)
		}	
		ZZ %<>%
			arrange()
		return(ZZ)
	} else {
		# if there are NO units that satisfy the condition, stop here and return the SRSWOR sample
		return(Z)
	}
}