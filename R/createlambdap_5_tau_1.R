#' Create the population, $\lambda_P$ = 5, $\tau$ = 1, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where $\lambda_P$ = 5, $\tau$ = 1.
#' @examples 
#' library(ggplot2)
#' data(lambdap_5_tau_1)
#' 
#' ggplot(lambdap_5_tau_1 %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))

#' @references
#' Christman, M. C. (1997). Efficiency of some sampling designs for spatially clustered populations. \emph{Environmetrics}, 8: 145--166.
#' @export

createlambdap_5_tau_1 <- function() {
	lambdap_5_tau_1 <- expand.grid(
		x = 1:10,
		y = 1:10
	) %>%
		cbind(
			y_value = c(
				rep(0,10),
				rep(0,4), 49, rep(0,5),
				rep(0,4), 1, rep(0,5),
				rep(0,10),
				rep(0,10),
				rep(0,7), 52, 5, 0,
				rep(0,8), 45, 0,
				rep(0,10),
				rep(0,10),
				rep(0,10)
			)
		)
	lambdap_5_tau_1_networks <- assignNetworkMembership(
		lambdap_5_tau_1 %>% filter(y_value > 0), 
		plot.size=1
	)
	# fill in m values
	lambdap_5_tau_1 %<>% merge(lambdap_5_tau_1_networks, all=T)
	lambdap_5_tau_1[which(lambdap_5_tau_1$y_value==0), ]$m <- 1
	# fill in NetworkIDs
	maxID <- max(lambdap_5_tau_1$NetworkID, na.rm=T)
	lambdap_5_tau_1[which(is.na(lambdap_5_tau_1$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
		to=(maxID + length(which(is.na(lambdap_5_tau_1$NetworkID)))), by=1)
	return(lambdap_5_tau_1)
}