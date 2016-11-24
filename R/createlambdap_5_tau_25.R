# create population
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
P = merge(lambdap_5_tau_25, lambdap_5_tau_25_networks, all=T)
P[which(P$y_value==0), ]$m <- 1
# fill in NetworkIDs
maxID <- max(P$NetworkID, na.rm=T)
P[which(is.na(P$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
	to=(maxID + length(which(is.na(P$NetworkID)))), by=1)

ggplot(P %>% filter(y_value!=0), aes(x, y)) + geom_text(aes(label=y_value))