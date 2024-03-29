#' Change NA values to 0.
#' @param x Vector of data.
#' @noRd
NA_is_Zero_Function <- function(x){	
     x[which(is.na(x))] <- 0
     return(x)
}

#' Return the sum of a vector, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @noRd
Sum <- function(x) {sum(x, na.rm=TRUE)}

#' Return the mean of a vector with up to two decimal places, after removing NAs.
#' 
#' @param x Vectors of data.
#' @description Written for use in the tables::tabular function to create publication-ready tables.
#' @noRd
Mean <- function(x) base::mean(x, na.rm=TRUE)


#' Population Variance
#' @param x vector of values 
#' @description Population variance
#' @noRd
popVar = function(x) {
     x = x[!is.na(x)]
     mu = mean(x)
     sum((x - mu)^2)/length(x)
}

#' Population Coefficient of Variation
#' 
#' @param x Vectors of data.
#' @noRd
popCV <- function(x) {
     sqrt(popVar(x))/Mean(x)
}

