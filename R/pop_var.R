#' Population Variance
#' @param x vector of values 
#' @description Population variance
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
pop_var = function(x) {
    x = x[!is.na(x)]
	mu = mean(x)
    mean((x - mu)^2)/length(x)
}
