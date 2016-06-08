#' Process Plot Surveys from Summer 2012
#' @description Process plot survey data collected within GTMNERR during Summer 2012. 
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @examples 
#' library(ggplot2)
#' PlotSurveys_season1 <- createPlotSurveys()
#' ggplot(PlotSurveys_season1 %>% filter(Island==6), aes(x,y, colour=NetworkID, label=unitID)) +
#' 	geom_point() + 
#' 	scale_colour_gradientn(colours = rainbow(7)) + 
#' 	geom_text(aes(label=unitID), hjust=0, vjust=0)

createPlotSurveys <- function() {
	Plot_Surveys <- SurveyNum <- Island <- Tag_Number <- Date <- NULL
	Network.mod <- Easting <- Northing <- Stricta <- Pusilla <- NULL
	CACA_on_Pusilla <- CACA_on_Stricta <- MEPR_on_Pusilla <-  NULL
	MEPR_on_Stricta <- Old_Moth_Evidence_Pusilla <- NULL 	
	Old_Moth_Evidence_Stricta <- Percent_Cover_Pusilla <- NULL
	Percent_Cover_Stricta <- Height_Pusilla <- Height_Stricta <- Cactus <- NULL
    # run munging files on Plot Info and Plot Surveys first
	PlotSurveys_season1 <- Plot_Surveys %>% 
		filter(SurveyNum==1) %>%
		# some plots were surveyed more than once
		group_by(Island, Tag_Number) %>%
		summarise(
			n.surveys = length(Date),
			Date = paste(Date, collapse=","),
			Network.mod = Network.mod[1],
			x = Easting[1],
			y = Northing[1],
			# cactus species presence
			Stricta = max(Stricta, na.rm=T),
			Pusilla = max(Pusilla, na.rm=T),
			Cactus = ifelse(sum(Stricta, Pusilla) > 0, 1, 0),
			# C. cactorum
			CACA_on_Pusilla = max(CACA_on_Pusilla, na.rm=T),
			CACA_on_Stricta = max(CACA_on_Stricta, na.rm=T),
			# M. prodenialis
			MEPR_on_Pusilla = max(MEPR_on_Pusilla, na.rm=T),
			MEPR_on_Stricta = max(MEPR_on_Stricta, na.rm=T),
			# Moth evidence
			Old_Moth_Evidence_Pusilla = max(Old_Moth_Evidence_Pusilla, na.rm=T),
			Old_Moth_Evidence_Stricta = max(Old_Moth_Evidence_Stricta, na.rm=T),
			# Cactus percent cover
			Percent_Cover_Pusilla = max(Percent_Cover_Pusilla, na.rm=T),
			Percent_Cover_Stricta = max(Percent_Cover_Stricta, na.rm=T),
			# Cactus max height
			Height_Pusilla = max(Height_Pusilla, na.rm=T),
			Height_Stricta = max(Height_Stricta, na.rm=T)	
			) %>%
			as.data.frame
	PlotSurveys_season1[,c(
		"Stricta",
		"Pusilla",
		"Cactus"
		)] %<>% 
		apply(2, NA_is_Zero_Function
	)
	cactus_species = c("Stricta", "Pusilla")
	variable = cbind(
		c(
			"CACA_on_Stricta",
			"MEPR_on_Stricta",
			"Old_Moth_Evidence_Stricta",
			"Percent_Cover_Stricta",
			"Height_Stricta"
		),
		c(
			"CACA_on_Pusilla",
			"MEPR_on_Pusilla",
			"Old_Moth_Evidence_Pusilla",
			"Percent_Cover_Pusilla",
			"Height_Pusilla"
		)
	)
	for (i in 1:length(cactus_species)) {
		for (j in 1:dim(variable)[2]) {
			PlotSurveys_season1[which(is.na(PlotSurveys_season1[,
				cactus_species[i]])), variable[j, i]] <- NA
		}
	}
	PlotSurveys_season1 %<>% filter(Cactus > 0) %>%
	as.data.table %>%
	setnames("Tag_Number", "unitID") %>%
	as.data.frame
	PlotSurveys_season1.network = createNetworks(PlotSurveys_season1)
	return(PlotSurveys_season1.network)
}