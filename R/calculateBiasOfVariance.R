#' calculate bias of the variance
#' @param dataset Dataset of simluation results
#' @param patch_data_summary Dataset summarizing known true parameters of the habitat patches sampled in the simulations
#' @param variables Variables for which to calculate bias
#' @param Sampling Defaults to "ACS"
#' @description calculate relative bias of the variance
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

calculateBiasOfVariance = function(dataset, patch_data_summary, variables, Sampling="ACS") {
	Plots <- n.networks <- N.SRSWOR.plots <- SamplingDesign <- funs <- . <- NULL
	mean_variables 		<- paste(variables, "_mean_observed", sep="")
	variance_variables 	<- paste(variables, "_var_observed", sep="")
	dataset %<>% 
		filter(Plots=="Horvitz Thompson Mean (All Plots)") %>%
		dplyr::select_(.dots=c(
			"n.networks", 
			"N.SRSWOR.plots",
			"SamplingDesign", 
			mean_variables, 
			variance_variables
		)) %>%
		group_by(n.networks, N.SRSWOR.plots, SamplingDesign) %>%
		summarise_each(
			funs(
				mean(., na.rm=T), 
				var(., na.rm=T)
			)
		) %>%
		as.data.frame
	variance_of_mean_variable <- paste(mean_variables, "_var", sep="")
	mean_of_variance_variable <- paste(variance_variables, "_mean", sep="")
	for (l in 1:length(variables)) {
		dataset %<>% mutate(
			VARNAME = (
				(eval(parse(text=paste(
					"dataset$", 
					mean_of_variance_variable[l], 
					sep=""
				))) - 
				eval(parse(text=paste(
					"dataset$",
					variance_of_mean_variable[l], 
					sep=""
				))))/
				eval(parse(text=paste(
					"dataset$",
					variance_of_mean_variable[l], 
					sep=""
				)))
			) * 100
		) %>%
		data.table::setnames(
			"VARNAME", 
			paste(variables[l], "relative_var_bias", sep="_")
		)
	}
	dataset %<>% 
	mutate(
		Plots = "HT",
		Sampling = Sampling
	) %>%
	merge(patch_data_summary, by="n.networks")
	return(dataset)	
}	