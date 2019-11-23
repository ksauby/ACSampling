#' Calculate SSQ_R
#' 
#' @description SSQ_R is the ratio of the within-network sum of squares to the total sum of squares. ACS design becomes more efficient relative to simple random sampling as the within-network variation increases relative to the overall variation [@thompson1996adaptive]. We calculate \deqn{SSQ_R = SSQ_w/SSQ_\tau}{SSQ_R = SSQ_w/SSQ_tau} as the ratio of the within-network sum of squares \deqn{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}, to the total sum of squares \deqn{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}. Thus, an increase in \code{SSQ_R} indicates an increase in the efficiency of the ACS design relative to SRSWOR.


#' @param popdata Information about the populations of interesting
#' @param popgroupvar variable identifying the separate populations. If only one population in popdata, set popgroupvar = NULL.
#' @param variable Variable for which to calculate SSQr?
#' @return Dataframe including original data and RE estimates.
#' @importFrom dplyr mutate_
#' @export
#' @references su2003estimator

data(Thompson1990Figure1Population)
temp <- Thompson1990Figure1Population %>%
	mutate(pop = 1)
popdata <- temp
variable <- "y_value"
popgroupvar <- "pop"


calculateSSQR <- function(popdata, variable, popgroupvar) {
	VAR <- sym(variable)
	if (is.null(popgroupvar)) {
		
		network_mean <- popdata %>%
			group_by(NetworkID) %>%
			summarise(
				network_mean = mean(!!VAR, na.rm = TRUE)
			)
		overall_mean <- popdata %>%
			summarise(
				overall_mean = mean(!!VAR, na.rm = TRUE)
			)
		variable_value %>% merge(
			network_mean, 
			by="NetworkID"
		) %>%
			merge(overall_mean) %>%
			mutate(
				SSQw_j = (.data$variable_value - .data$network_mean)^2,
				SSQt_i = (.data$variable_value - .data$overall_mean)^2
			) %>%
			group_by_(.dots=popgroupvar) %>%
			summarise(
				SSQw = sum(.data$SSQw_j),
				SSQt = sum(.data$SSQt_i)
			) %>%
			mutate(
				SSQ_R = .data$SSQw / .data$SSQt,
				variable_name = variable
			) %>%
			dplyr::select(-c(.data$SSQw, .data$SSQt))
	} else {
		variable_value <- popdata %>%
			mutate_(
				variable_value = interp(
					~var, 
					var = as.name(variable)
				)
			)
		network_mean <- popdata %>%
			group_by_(.dots = c(popgroupvar, "NetworkID")) %>%
			summarise_(
				network_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = as.name(variable)
				)
			)
		overall_mean <- popdata %>%
			group_by_(.dots=popgroupvar) %>%
			summarise_(
				overall_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(variable)
				)
			)
		variable_value %>% merge(
			network_mean, 
			by=c(popgroupvar, "NetworkID")
		) %>%
			merge(overall_mean, by = popgroupvar) %>%
			mutate(
				SSQw_j = (variable_value - network_mean)^2,
				SSQt_i = (variable_value - overall_mean)^2
			) %>%
			group_by_(.dots=popgroupvar) %>%
			summarise(
				SSQw = sum(.data$SSQw_j),
				SSQt = sum(.data$SSQt_i)
			) %>%
			mutate(
				SSQ_R = .data$SSQw / .data$SSQt,
				variable_name = variable
			) %>%
			dplyr::select(-c(.data$SSQw, .data$SSQt))
	}
}
	
	
	
	