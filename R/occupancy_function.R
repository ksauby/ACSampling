#' Occupancy Function
#' @param x vector of presence/absence data
#' @description For multiple observations of the sample plot, determine how many presences were observed.
occupancyfunction <- function(x) {
	ifelse(sum(x, na.rm=T) > 0, sum(x, na.rm=T), 0)
}