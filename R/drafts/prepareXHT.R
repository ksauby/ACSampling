#' Prepare data for estimation of the mean using x_HT
#' 
#' @param dataset dataset
#' @param variables variables to summarize
#' @param

O <- alldata %>% 
	filter(Sampling!="Edge") %>%
	.[, c(
		variables, 
		"NetworkID", 
		"m"
	), with=FALSE]