#' Create the Table 1 data from Thompson (1990)
#' 
#' @return The data displayed in Table 1 from Thompson (1990).
#' @examples createThompson1990Table1data()
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' Thompson, S. (1990). Adaptive Cluster Sampling. \emph{Journal of the American Statistical Association}, 85(412): 1050--1059.
#' @importFrom utils combn
#' @export

createThompson1990Table1data <- function() {
	sampling_effort <- sampling <- NULL
	N=5
	n1=2
	m_k=c(1,1,1,2,2)
	y_value=c(1,0,2,10,1000)
	m_k_y = cbind(m_k, y_value)
	# all possible combos for n1 = 2
	combos = combn(y_value, 2) %>%
		t %>%
		as.data.frame
	temp = as.list(data.frame(t(combos)))
	temp = lapply(temp, as.data.frame)
	# rename column 1
	temp <- lapply(seq(temp), function(i) {
		y <- data.frame(temp[[i]])
		names(y)[1] <- c("y_value")
		y %<>% merge(m_k_y)
		return(y)
	})
	# add column 2 - sampling effort number
	for (i in 1:length(temp)) {
		temp[[i]]$sampling_effort = i
		temp[[i]]$sampling = "SRSWOR"
	}
	# choose adaptive samples
	for (i in 1:length(temp)) {
		if (10 %in% temp[[i]]$y_value & !(1000 %in% temp[[i]]$y_value)) 
			{temp[[i]][dim(temp[[i]])[1] + 1, ] = c(1000, 2, i, "ACS")}
		if (1000 %in% temp[[i]]$y_value & !(10 %in% temp[[i]]$y_value)) 
			{temp[[i]][dim(temp[[i]])[1] + 1, ] = c(10, 2, i, "ACS")}
		if (10 %in% temp[[i]]$y_value & !(2 %in% temp[[i]]$y_value)) 
			{temp[[i]][dim(temp[[i]])[1] + 1, ] = c(2, 0, i, "ACS")}
	}
	temp %<>% 
		do.call(rbind.data.frame, .) %>%
		arrange(.data$sampling_effort)
	temp$y_value %<>% as.integer
	temp$m_k %<>% as.integer
	temp$sampling_effort %<>% as.numeric
	temp %<>% 
		select(
		     .data$sampling_effort, 
		     .data$sampling, 
		     .data$y_value, 
		     .data$m_k) %>%
		arrange(
		     .data$sampling_effort, 
		     rev(.data$sampling), 
		     .data$y_value
          )
	return(temp)
}
