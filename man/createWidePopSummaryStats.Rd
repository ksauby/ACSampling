% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createWidePopSummaryStats.R
\name{createWidePopSummaryStats}
\alias{createWidePopSummaryStats}
\title{Create "Wide" Format Population Summary Statistics for Population Data}
\usage{
createWidePopSummaryStats(popsummarystats, ovar, rvar)
}
\arguments{
\item{popsummarystats}{Created by the function calcPopSummaryStats.}

\item{ovar}{vector of occupancy variables}

\item{rvar}{vector of ratio variables}
}
\value{
Dataframe with a column per mean/variance of each variable and a row per population.
}
\description{
Create "Wide" Format Population Summary Statistics for Population Data
}
\examples{
#CactusRealizationSummary <- calcPopSummaryStats(
#	popdata = CactusRealizations, 
#	summaryvar = c("Stricta", "Pusilla", "Cactus",
#		"MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#		"Height_Stricta", "Old_Moth_Evidence_Stricta"), 
#	popvar = "population", 
#	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", 
#		"Percent_Cover_Stricta", "Height_Stricta", 
#		"Old_Moth_Evidence_Stricta"),
#	nrow=30,
#	ncol=30
#)
#patch_data_summary_wide <- createWidePopSummaryStats(
#	popsummarystats = CactusRealizationSummary,
#	ovar = "Stricta",
#	rvar = c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta", 
#		"Height_Stricta", "Old_Moth_Evidence_Stricta")
#)
}
