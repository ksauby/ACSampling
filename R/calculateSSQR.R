#' Calculate SSQ_R
#' 
#' @description SSQ_R is the ratio of the within-network sum of squares to the total sum of squares. The adaptive cluster sampling (ACS) design becomes more efficient relative to simple random sampling (SRS) as the within-network variation increases relative to overall variation [@thompson1996adaptive]. We calculate \deqn{SSQ_R = SSQ_w/SSQ_\tau}{SSQ_R = SSQ_w/SSQ_tau} as the ratio of the within-network sum of squares \deqn{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}{SSQ_w = \sum_{j=1}^{\kappa} \sum_{i \in j} (y_{j,i} - \bar{y_{j}})^2}, to the total sum of squares \deqn{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}{SSQ_\tau= \sum_{i=1}^{N} (y_i - \mu)^2}. Thus, an increase in \code{SSQ_R} indicates an increase in the efficiency of the ACS design relative to SRS.


#' @param popdata Information about the populations of interest. MORE DETAIL ABOUT STRUCTURE OF THIS.
#' @template popvar_default_null
#' @param variable Variable for which to calculate SSQr.
#' @return Dataframe including original data and RE estimates.
#' @export
#' @references 
#' \insertRef{su2003estimator}{ACSampling}
#' @examples
#' library(magrittr)
#' library(dplyr)
#' data(Thompson1990Fig1Pop)
#' temp <- Thompson1990Fig1Pop %>%
#' 	mutate(pop = 1)
#' popdata <- temp
#' variable <- "y_value"
#' popvar <- "pop"
#' # calcSSQR(popdata, variable, popvar)
#' popvar <- NA
#' # calcSSQR(popdata, variable, popvar)


calcSSQR <- function(popdata, variable, popvar=NULL) {
	VAR <- sym(variable)
	if (!(is.na(popvar))) {
		POPVAR <- sym(popvar)
	}
	if (is.na(popvar)) {
		network_mean <- popdata %>%
			group_by(.data$NetworkID) %>%
			summarise(
				network_mean = mean(!!VAR, na.rm = TRUE)
			)
		overall_mean <- popdata %>%
			summarise(
				overall_mean = mean(!!VAR, na.rm = TRUE)
			)
		popdata %>% merge(
			network_mean, 
			by="NetworkID"
		) %>%
			merge(overall_mean) %>%
			mutate(
				SSQw_j = (!!VAR - .data$network_mean)^2,
				SSQt_i = (!!VAR - .data$overall_mean)^2
			) %>%
			group_by(.data[[popvar]]) %>%
			summarise(
				SSQw = sum(.data$SSQw_j),
				SSQt = sum(.data$SSQt_i)
			) %>%
			mutate(
				SSQ_R = .data$SSQw / .data$SSQt
			) %>%
			dplyr::select(-c(.data$SSQw, .data$SSQt))
	} else {
		network_mean <- popdata %>%
			group_by(NetworkID, !!POPVAR) %>%
			summarise(
				network_mean = mean(!!VAR, na.rm = TRUE)
			)
		overall_mean <- popdata %>%
			group_by(!!POPVAR) %>%
			summarise(
				overall_mean = mean(!!VAR, na.rm = TRUE)
			)
		popdata %>% merge(
			network_mean, 
			by=c(popvar, "NetworkID")
		) %>%
			merge(overall_mean, by = popvar) %>%
			mutate(
				SSQw_j = (!!VAR - network_mean)^2,
				SSQt_i = (!!VAR - overall_mean)^2
			) %>%
			group_by(!!POPVAR) %>%
			summarise(
				SSQw = sum(.data$SSQw_j),
				SSQt = sum(.data$SSQt_i)
			) %>%
			mutate(
				SSQ_R = .data$SSQw / .data$SSQt,
				variable_name = variable
			) %>%
			dplyr::select(-c(
                    .data$SSQw, 
                    .data$SSQt, 
                    .data$variable_name
               ))
	}
}