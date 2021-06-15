#' Population Variance
#' @param x vector of values 
#' @description Population variance
#' @noRd
pop_var = function(x) {
    x = x[!is.na(x)]
	mu = mean(x)
    mean((x - mu)^2)/length(x)
}
