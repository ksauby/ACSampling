#' t_HT_function Function
#' @param x 
#' @description Population variance
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
t_HT_function <- function(x) {
	ACSampling::t_HT(N=N, n1=n1, mk=mk, y=x, sampling=sampling, criterion=0)
}
