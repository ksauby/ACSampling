#' Create the population, \deqn{\lambda_P = 5, \tau = 1}{lambda_P = 5, tau = 1}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 5, \tau = 1}{lambda_P = 5, tau = 1}.
#' @noRd
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
		lambdap_5_tau_1 %>% filter(.data$y_value > 0), 
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




#' Create the population, \deqn{\lambda_P = 5, \tau = 5}{lambda_P = 5, tau = 5}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 5, \tau = 5}{lambda_P = 5, tau = 5}.
#' @noRd

createlambdap_5_tau_5 <- function() {
     lambdap_5_tau_5 <- expand.grid(
          x = 1:10,
          y = 1:10
     ) %>%
          cbind(
               y_value = c(
                    0,2,1,0,0,1,9,0,0,0,
                    0,20,13,1,1,2,24,4,1,13,
                    1,7,7,1,0,4,20,4,7,26,
                    rep(0,5),5,14,2,2,0,
                    rep(0,6),3,0,0,0,
                    rep(0,10),
                    rep(0,10),
                    rep(0,10),
                    rep(0,4),2,5,1,0,0,0,
                    rep(0,4),16,20,rep(0,4)
               )
          )
     lambdap_5_tau_5_networks <- assignNetworkMembership(
          lambdap_5_tau_5 %>% filter(.data$y_value > 0), 
          plot.size=1
     )
     # fill in m values
     lambdap_5_tau_5 %<>% merge(lambdap_5_tau_5_networks, all=T)
     lambdap_5_tau_5[which(lambdap_5_tau_5$y_value==0), ]$m <- 1
     # fill in NetworkIDs
     maxID <- max(lambdap_5_tau_5$NetworkID, na.rm=T)
     lambdap_5_tau_5[which(is.na(lambdap_5_tau_5$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
                                                                                 to=(maxID + length(which(is.na(lambdap_5_tau_5$NetworkID)))), by=1)	
     return(lambdap_5_tau_5)
}




#' Create the population, \deqn{\lambda_P = 5, \tau = 10}{lambda_P = 5, tau = 10}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 5, \tau = 10}{lambda_P = 5, tau = 10}.
#' @noRd

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





#' Create the population, \deqn{\lambda_P = 5, \tau = 25}{lambda_P = 5, tau = 25}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 5, \tau = 25}{lambda_P = 5, tau = 25}.
#' @noRd

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
          lambdap_5_tau_25 %>% filter(.data$y_value > 0), 
          plot.size=1
     )
     # fill in m values
     lambdap_5_tau_25 %<>% merge(lambdap_5_tau_25_networks, all=T)
     lambdap_5_tau_25[which(lambdap_5_tau_25$y_value==0), ]$m <- 1
     # fill in NetworkIDs
     maxID <- max(lambdap_5_tau_25$NetworkID, na.rm=T)
     lambdap_5_tau_25[which(is.na(lambdap_5_tau_25$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
                                                                                   to=(maxID + length(which(is.na(lambdap_5_tau_25$NetworkID)))), by=1)
     return(lambdap_5_tau_25)
}





#' Create the population, \deqn{\lambda_P = 10, \tau = 5}{lambda_P = 10, tau = 5}, from Christman (1997)
#' 
#' @return The population displayed in Christman (1997), where \deqn{\lambda_P = 10, \tau = 5}{lambda_P = 10, tau = 5}.
#' @noRd

createlambdap_10_tau_5 <- function() {
     lambdap_10_tau_5 <- expand.grid(
          x = 1:10,
          y = 1:10
     ) %>%
          cbind(
               y_value = c(
                    0,0,0,1,14,23,0,0,14,37,
                    rep(0,5),4,0,0,0,5,
                    rep(0,8),1,1,
                    rep(0,8),3,7,
                    8,4,0,6,2,2,1,0,23,27,
                    17,20,3,12,22,4,0,2,8,42,
                    3,2,1,4,12,1,3,23,20,5,
                    rep(0,5),14,19,26,35,7,
                    14,3,0,0,8,25,29,17,26,10,
                    36,6,1,0,4,4,35,39,8,5
               )
          )
     lambdap_10_tau_5_networks <- assignNetworkMembership(
          lambdap_10_tau_5 %>% filter(.data$y_value > 0), 
          plot.size=1
     )
     # fill in m values
     lambdap_10_tau_5 %<>% merge(lambdap_10_tau_5_networks, all=T)
     lambdap_10_tau_5[which(lambdap_10_tau_5$y_value==0), ]$m <- 1
     # fill in NetworkIDs
     maxID <- max(lambdap_10_tau_5$NetworkID, na.rm=T)
     lambdap_10_tau_5[which(is.na(lambdap_10_tau_5$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
                                                                                   to=(maxID + length(which(is.na(lambdap_10_tau_5$NetworkID)))), by=1)
     return(lambdap_10_tau_5)
}





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