#' Create the population, \deqn{\lambda_P = 5, \tau = 10}{lambda_P = 5, tau = 10}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 5, \tau = 10}{lambda_P = 5, tau = 10}.
#' @examples 
#' library(ggplot2)
#' data(lambdap_5_tau_10)
#' 
#' ggplot(lambdap_5_tau_10 %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))

#' @references
#' Christman, M. C. (1997). Efficiency of some sampling designs for spatially clustered populations. \emph{Environmetrics}, 8: 145--166.
#' @export

createlambdap_5_tau_10 <- function() {
	lambdap_5_tau_10 <- expand.grid(
		x = 1:10,
		y = 1:10
	) %>%
		cbind(
			y_value = c(
				rep(0,4),3,10,22,16,7,7,
				rep(0,3),2,1,10,10,13,16,9,
				0,0,1,5,1,5,8,5,8,6,
				2,1,4,9,8,1,0,1,1,1,
				1,6,7,9,3,5,1,1,2,0,
				0,4,8,6,5,2,4,3,2,2,
				1,1,rep(0,4),3,9,8,3,
				0,0,0,2,0,0,0,2,3,1,
				rep(0,7),1,0,0,
				rep(0,10)
			)
		)
	lambdap_5_tau_10_networks <- assignNetworkMembership(
		lambdap_5_tau_10 %>% filter(.data$y_value > 0), 
		plot.size=1
	)
	# fill in m values
	lambdap_5_tau_10 %<>% merge(lambdap_5_tau_10_networks, all=T)
	lambdap_5_tau_10[which(lambdap_5_tau_10$y_value==0), ]$m <- 1
	# fill in NetworkIDs
	maxID <- max(lambdap_5_tau_10$NetworkID, na.rm=T)
	lambdap_5_tau_10[which(is.na(lambdap_5_tau_10$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
		to=(maxID + length(which(is.na(lambdap_5_tau_10$NetworkID)))), by=1)
	return(lambdap_5_tau_10)
}