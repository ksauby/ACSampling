#' Create an Adaptive Cluster Sample
#'
#' @template popdata
#' @template seed
#' @template n1
#' @template yvar
#' @template criterion
#' @template initsample

#' @return A restricted adaptive cluster sample.

#' @examples
#' library(ggplot2)
#' data(Thompson1990Fig1Pop)
#' data(Thompson1990Figure1Sample)
#' # Create ACS sample, seed=9
#' # - in the dataframe "Thompson1990Fig1Pop", the variable of interest $y$ is "y_value"
#' # - any "y_value" greater than the criterion 0 will trigger cluster sampling
#' popdata = Thompson1990Fig1Pop
#' seed = 9
#' n1 = 10
#' yvar = "y_value"
#' criterion = 0
#' Z = createACS(popdata, n1, yvar, criterion, seed)
#' # plot ACS sample and population from which sampled was collected. In the plot, the open squares correspond to population units that were sampled
#' ggplot() +
#' geom_point(
#' data=Thompson1990Fig1Pop,
#' aes(x, y, size=y_value,shape=factor(y_value))) +
#' scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' geom_point(data=Z, aes(x,y), shape=0, size=7)
#' # Create another ACS, seed=26
#' # - In this example, no units satisfy the criterion and thus cluster samplingd oes not occur
#' Z = createACS(
#' popdata=Thompson1990Fig1Pop,
#' seed=26,
#' n1=10,
#' yvar="y_value",
#' criterion=0
#' )
#' # plot ACS sample and population from which sampled was collected
#' ggplot() +
#' geom_point(
#' data=Thompson1990Fig1Pop,
#' aes(x, y, size=factor(y_value), shape=factor(y_value))
#' ) +
#' scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
#' geom_point(data=Z, aes(x,y), shape=0, size=7)

#' @template Thompson1990

#' @export
#' @importFrom stringr str_pad
#' @importFrom dplyr filter rowwise
#' @importFrom ggplot2 ggplot

createACS <- function(popdata, n1, yvar, criterion=0, seed=NA, initsample=NA) {

     handleError_popdata(popdata)
     handleError_n1(n1)
     handleError_yvar(yvar)
     handleError_seed(seed)
     handleError_criterion(criterion)
     
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
	Networks = Z %>% filter(!!YVAR > criterion)
	# if there are units that satisfy the criterion, fill in edge units
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
			filter(row_number()==1) %>%
		     ungroup()
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
		# if there are NO units that satisfy the criterion, stop here and return the SRSWOR sample
		return(Z)
	}
}