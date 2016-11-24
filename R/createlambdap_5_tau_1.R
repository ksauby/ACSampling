# create population
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
P = merge(lambdap_5_tau_1, lambdap_5_tau_1_networks, all=T)
P[which(P$y_value==0), ]$m <- 1
# fill in NetworkIDs
maxID <- max(P$NetworkID, na.rm=T)
P[which(is.na(P$NetworkID)), ]$NetworkID <- seq(from=(maxID + 1), 
	to=(maxID + length(which(is.na(P$NetworkID)))), by=1)

ggplot(P, aes(x, y)) + geom_text(aes(label=y_value))