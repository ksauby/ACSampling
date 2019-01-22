#' Change NA values to 0.
#'
#' @param x Vector of data.

NA_is_Zero_Function <- function(x){	
	x[which(is.na(x))] <- 0
	return(x)
}





#' Create Cactus Realization Data
#' 
#' @param PlotSurveys_season1 Plot survey data from season 1.
#' @param ovar occupancy variables.
#' @return The data from Sauby and Christman.
#' @examples 
#' library(ggplot2)
#' CactusRealizations <- createCactusRealizations()
#' ggplot(
#' 	CactusRealizations,
#' 	aes(
#' 		x, 
#' 		y, 
#' 		shape = factor(Cactus)
#' )) +
#' 	geom_point() +
#' 	facet_wrap(~population) +
#' 	scale_shape_manual(values=c(4,16))
#' 
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

createCactusRealizations <- function(PlotSurveys_season1, ovar) {
	# REALIZATION DATA
	x_start 		<- 1
	x_end 			<- 30
	y_start 		<- 1
	y_end 			<- 30
	n.networks 		<- c(5, 15, 10, 20, 30, 40)
	n.realizations 	<- 1
	SpeciesInfo 	<- PlotSurveys_season1
	buffer			<- 5
	start.seed		<- 1
	variables		<- ovar
	# CREATE REALIZATIONS
	patch_data <- createSpeciesPatchRealizations(
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
	colnames(patch_data)[names(patch_data) == "n.networks"] <- "population"
	patch_data$population %<>% as.factor()
	levels(patch_data$population) <- 1:6
	# convert NAs to zeros
	 patch_data[,c(
	 	"CACA_on_Pusilla",
	 	"CACA_on_Stricta",
	 	"MEPR_on_Pusilla",
	 	"MEPR_on_Stricta",
	 	"Old_Moth_Evidence_Pusilla",
	 	"Old_Moth_Evidence_Stricta",
	 	"Percent_Cover_Pusilla",
	 	"Percent_Cover_Stricta",
	 	"Height_Pusilla",
	 	"Height_Stricta"
	 )] %<>% apply(.data, 2, NA_is_Zero_Function)
	 return(patch_data)
}