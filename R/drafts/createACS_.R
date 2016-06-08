# for use in simulations

data(Thompson1990Figure1Population)
data(Thompson1990Figure1Sample)

# Initiate ACS
ACS1 = createACS(Thompson1990Figure1Population, seed=26, n1=10, "y_value", condition=0) %>% mutate(Function="createACS")
ACS2 = createACS_(pop=Thompson1990Figure1Population, seed=26, n1=10) %>% mutate(Function="createACS_")
ACS_example = rbind(ACS1, ACS2)


# plot ACS sample overlaid onto population
ggplot() +
	geom_point(data=Thompson1990Figure1Population, aes(x,y, size=factor(y_value),
		shape=factor(y_value))) +
	scale_shape_manual(values=c(1, rep(16, length(2:13)))) +
	geom_point(data=ACS2, aes(x,y), shape=0, size=7) + 
	facet_wrap(~Function)



createACS_ <- function(pop, seed=1, n1) {
	S <- createSRSWOR(pop, seed, n1)
	# add the rest of the units for each network in the initial sample
	S %<>% as.data.table
	Z = S %>% 
		rbind.fill(population[which(pop$NetworkID %in% S$NetworkID), ]) %>% 
		arrange(x,y)
		as.data.table %>%
		unique
	return(Z)
}