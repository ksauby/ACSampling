#' Create the population, $\lambda_P$ = 5, $tau$ = 10, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where $\lambda_P$ = 5, $tau$ = 10.
#' @examples 
#' library(ggplot2)
#' data(lambdap_5_tau_25)
#' 
#' ggplot(lambdap_5_tau_25 %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))


#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' Christman, M. C. (1997). Efficiency of some sampling designs for spatially clustered populations. \emph{Environmetrics}, 8: 145--166.
#' @export

createlambdap_5_tau_25 <- function() {
	lambdap_5_tau_25 <- expand.grid(
		x = 1:10,
		y = 1:10
	) %>%
		cbind(
			y_value = c(
				0,0,0,4,rep(0,6),
				0,0,0,43,17,rep(0,5),
				rep(0,10),
				rep(0,10),
				rep(0,10),
				rep(0,2), rep(1,4), rep(0,4),
				rep(0,2), 50, 51, 41, 5, rep(0,4),
				rep(0,2), 2, 1, rep(0,6),
				rep(0,10),
				rep(0,10)
			)
		)
	lambdap_5_tau_25_networks <- assignNetworkMembership(
		lambdap_5_tau_25 %>% filter(y_value > 0), 
		plot.size=1
	)
	# fill in m values
	lambdap_5_tau_25 %<>% merge(lambdap_5_tau_25_networks, all=T)
	lambdap_5_tau_25[which(lambdap_5_tau_25$y_value==0), ]$m <- 1
	# fill in NetworkIDs
	maxID <- max(lambdap_5_tau_25$NetworkID, na.rm=T)
	lambdap_5_tau_25[which(is.na(lambdap_5_tau_25$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
		to=(maxID + length(which(is.na(lambdap_5_tau_25$NetworkID)))), by=1)

	ggplot(lambdap_5_tau_25 %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))
	return(lambdap_5_tau_25)
}