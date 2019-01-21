#' Calculate SSQ_R
#' 
#' @description SSQ_R is the ratio of the within-network sum of squares to the total sum of squares. ACS design becomes more efficient relative to simple random sampling as the within-network variation increases relative to the overall variation [@thompson1996adaptive]. We calculate \deqn{SSQ_R = SSQ_w/SSQ_\tau}{SSQ_R = SSQ_w/SSQ_tau} as the ratio of the within-network sum of squares \deqn{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}, to the total sum of squares \deqn{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}. Thus, an increase in \code{SSQ_R} indicates an increase in the efficiency of the ACS design relative to SRSWOR.


#' @param patch_data Information about the populations of interesting
#' @param population.grouping.variable variable identifying the separate populations. If only one population in patch_data, set population.grouping.variable = NULL.
#' @param variable Variable for which to calculate SSQr?
#' @return Dataframe including original data and RE estimates.
#' @importFrom dplyr mutate_
#' @export
#' @references su2003estimator

calculateSSQR <- function(patch_data, variable, population.grouping.variable) {
	if (is.null(population.grouping.variable)) {
		variable_value <- patch_data %>%
			mutate_(
				variable_value = interp(
					~var, 
					var = as.name(variable)
				)
			)
		network_mean <- patch_data %>%
			group_by(NetworkID) %>%
			summarise_(
				network_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = as.name(variable)
				)
			)
		overall_mean <- patch_data %>%
			summarise_(
				overall_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(variable)
				)
			)
		variable_value %>% merge(
			network_mean, 
			by="NetworkID"
		) %>%
			merge(overall_mean) %>%
			mutate(
				SSQw_j = (variable_value - network_mean)^2,
				SSQt_i = (variable_value - overall_mean)^2
			) %>%
			group_by_(.dots=population.grouping.variable) %>%
			summarise(
				SSQw = sum(SSQw_j),
				SSQt = sum(SSQt_i)
			) %>%
			mutate(
				SSQ_R = SSQw / SSQt,
				variable_name = variable
			) %>%
			dplyr::select(-c(SSQw, SSQt))
	} else {
		variable_value <- patch_data %>%
			mutate_(
				variable_value = interp(
					~var, 
					var = as.name(variable)
				)
			)
		network_mean <- patch_data %>%
			group_by_(.dots = c(population.grouping.variable, "NetworkID")) %>%
			summarise_(
				network_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = as.name(variable)
				)
			)
		overall_mean <- patch_data %>%
			group_by_(.dots=population.grouping.variable) %>%
			summarise_(
				overall_mean = interp(
					~mean(var, na.rm = TRUE), 
					var = 
					as.name(variable)
				)
			)
		variable_value %>% merge(
			network_mean, 
			by=c(population.grouping.variable, "NetworkID")
		) %>%
			merge(overall_mean, by = population.grouping.variable) %>%
			mutate(
				SSQw_j = (variable_value - network_mean)^2,
				SSQt_i = (variable_value - overall_mean)^2
			) %>%
			group_by_(.dots=population.grouping.variable) %>%
			summarise(
				SSQw = sum(SSQw_j),
				SSQt = sum(SSQt_i)
			) %>%
			mutate(
				SSQ_R = SSQw / SSQt,
				variable_name = variable
			) %>%
			dplyr::select(-c(SSQw, SSQt))
	}
}
	
	
	
	