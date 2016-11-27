#' Create the population, $\lambda_P$ = 5, $tau$ = 10, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where $\lambda_P$ = 5, $tau$ = 10.
#' @examples 
#' library(ggplot2)
#' data(lambdap_10_tau_25)
#' 
#' ggplot(lambdap_10_tau_25 %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))


#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' Christman, M. C. (1997). Efficiency of some sampling designs for spatially clustered populations. \emph{Environmetrics}, 8: 145--166.
#' @export

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
		lambdap_10_tau_25 %>% filter(y_value > 0), 
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