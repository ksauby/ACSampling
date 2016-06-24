#' Prepare data for estimation of the variance of the mean using var_x_HT
#' 
#' @param dataset dataset
#' @param variables variables to summarize
#' @param



O_smd <- alldata %>% 
	.[, c(
		paste(variables, "_network_sum", sep=""), 
		"NetworkID", 
		"m"
	), with=FALSE] %>% 
	filter(!(is.na(NetworkID))) %>%
	.[, lapply(.SD, function(x) {x[1]}), by=NetworkID]