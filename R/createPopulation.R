#' Create a population of locations (in a grid). 
#' 
#' @template x_start
#' @template x_end
#' @template y_start
#' @template y_end
#' @return A grid of locations with coordinates (x, y).
#' @export
#' @description Create a dataframe of all x and y coordinates (as integer values) within a rectangular or square grid of coordinates.

#' @examples
#' population <- createPop(
#' x_start = 5, 
#' x_end = 27, 
#' y_start = 5, 
#' y_end = 27
#' )

createPop <- function(x_start, x_end, y_start, y_end) {
     
     handleError_coord(x_start)
     handleError_coord(x_end)
     handleError_coord(y_start)
     handleError_coord(y_end)
     
     data.frame(expand.grid(
          x = 	x_start:x_end,
          y = 	y_start:y_end
     )) # %>%
     # mutate(unitID = 1:length(x))
}
