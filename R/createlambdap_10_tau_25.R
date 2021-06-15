#' Create the population, \deqn{\lambda_P = 10, \tau = 25}{lambda_P = 10, tau = 25}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 10, \tau = 25}{lambda_P = 10, tau = 25}.
#' @noRd
createlambdap_10_tau_25 <- function() {
	lambdap_10_tau_25 <- expand.grid(
		x = 1:10,
		y = 1:10
	) %>%
		cbind(
			y_value = c(
				rep(0,5),10,1,0,0,0,
				0,0,2,10,18,32,5,rep(0,3),
				0,0,30,26,16,1,34,1,0,7,
				0,0,0,0,12,25,7,0,9,42,
				0,0,0,0,1,21,1,2,12,0,
				9,0,0,49,33,15,2,3,26,9,
				43,0,0,7,5,0,0,0,0,34,
				1,rep(0,9),
				46,5,0,1,9,2,35,2,0,0,
				1,0,0,1,44,1,15,1,0,0
			)
		)
	lambdap_10_tau_25_networks <- assignNetworkMembership(
		lambdap_10_tau_25 %>% filter(.data$y_value > 0), 
		plot.size=1
	)
	# fill in m values
	lambdap_10_tau_25 %<>% merge(lambdap_10_tau_25_networks, all=T)
	lambdap_10_tau_25[which(lambdap_10_tau_25$y_value==0), ]$m <- 1
	# fill in NetworkIDs
	maxID <- max(lambdap_10_tau_25$NetworkID, na.rm=T)
	lambdap_10_tau_25[which(is.na(lambdap_10_tau_25$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
		to=(maxID + length(which(is.na(lambdap_10_tau_25$NetworkID)))), by=1)
	return(lambdap_10_tau_25)
}