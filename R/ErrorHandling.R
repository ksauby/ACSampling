# must be an integer
handleError_n1 <- function(n1) {
     if (is.numeric(n1)==TRUE) {
          if (length(n1) > 1) {
               stop(
                    "The argument 'n1' must be an integer value.",
                    call.=FALSE
               )
          } else if (round(n1)!=n1) {
               stop(
                    "The argument 'n1' must be an integer value.",
                    call.=FALSE
               )
          }
     } else {
          stop(
               "The argument 'n1' must be an integer value.",
               call.=FALSE
          )
     }
}

handleError_n1vector <- function(n1) {
     if (is.numeric(n1)==TRUE) {
          n2 <- round(n1)
           if (!identical(n2,n1)) {
               stop(
                    "The argument 'n1' must be an integer value or vector of integers.",
                    call.=FALSE
               )
          }
     } else {
          stop(
               "The argument 'n1' must be an integer value or vector of integers.",
               call.=FALSE
          )
     }
}

# must be a number
handleError_condition <- function(condition) {
     if (is.numeric(condition) == FALSE) {
          stop("The argument 'condition' must be a numeric value.",
               call. = FALSE)
     } else if (length(condition) > 1) {
          stop("The argument 'condition' must be a single numeric value.",
               call. = FALSE)
     }
}
handleError_coord <- function(coord) {
     if (is.numeric(coord)==FALSE) {
          stop(
               "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
               call.=FALSE
          )
     } else if (length(coord) > 1) {
          stop(
               "A non-numeric value was passed to one of the coordinate arguments. Please provide a number.",
               call.=FALSE
          )
     }
}  
# must be a dataframe with x, y, and NetworkID columns; x and y must be numeric
handleError_popdata <- function(popdata) {
     if (is.data.frame(popdata)==FALSE) {
          stop(
               "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'.",
               call.=FALSE
          )
     } else if (all(c("x", "y", "NetworkID") %in% names(popdata)) == FALSE) {
          stop(
               "The 'popdata' argument must be supplied with a data frame including the columns 'x', 'y', and 'NetworkID'.",
               call.=FALSE
          )
     }
     # WHAT ABOUT WHEN ITS A DATA FRAME BUT X AND Y ARE NOT IN THE RIGHT FORMATS?
}
# must be integer(s)
handleError_seed <- function(seed) {
     if (all(is.numeric(seed))==TRUE) {
          if (!isTRUE(all(seed == floor(seed)))) {
               stop(
                    "The 'seed' argument must be a vector of integer values.",
                    call.=FALSE
               )
          }
     } else if (!is.na(seed)) {
          stop(
               "The 'seed' argument must be a vector of integer values.",
               call.=FALSE
          )
     }
}

# must be character
handleError_yvar <- function(yvar) {
     if (is.character(yvar)==FALSE) {
          stop(
               "The argument 'yvar' must be a character string.",
               call.=FALSE
          )
     } else if (length(yvar) > 1) {
          stop(
               "The argument 'yvar' must be a character string.",
               call.=FALSE
          )    
     }
}

handleError_variable <- function(variable, argument) {
     if (is.character(variable)==FALSE) {
          stop(
               paste(
                    "The argument '",
                    argument,
                    "' must be a character string or a vector of character strings.",
                    sep=""
               ),
               call.=FALSE
          )
     }
}

# must be character or vector of characters
handleError_vars <- function(vars) {
     if (is.null(vars)==TRUE | all(is.na(vars))==TRUE) {
          stop(
               "At least one variable, via the arguments 'ovar', 'avar', or 'rvar', must be supplied for estimation.",
               call.=FALSE
          )
     }
}

# must be logical, character vector of length 1
handleError_LogicalVar <- function(LogicalVar, argument) {
     if (length(LogicalVar) > 1) {
          stop(
               paste(
                    "The argument '",
                    argument,
                    "' must be assigned a value of either TRUE or FALSE.",
                    sep=""
               ),
               call.=FALSE
          )    
     } else if (is.logical(LogicalVar)==FALSE) {
          stop(
               paste(
                    "The argument '",
                    argument,
                    "' must be assigned a value of either TRUE or FALSE.",
                    sep=""
               ),
               call.=FALSE
          )    
     } else if (is.na(LogicalVar)) {
          stop(
               paste(
                    "The argument '",
                    argument,
                    "' must be assigned a value of either TRUE or FALSE.",
                    sep=""
               ),
               call.=FALSE
          )    
     }
}



