#' Create a population of locations (in a grid). 
#' 
#' @param x_start The most western coordinate of the grid.
#' @param x_end The most eastern coordinate of the grid.
#' @param y_start The most southern coordinate of the grid.
#' @param y_end The most northern coordinate of the grid.
#' @return A grid of locations with coordinates (x, y).
#' @examples population <- createPopulation(x_start = 5, x_end = 27, y_start = 5, y_end = 27)
#' @references Sauby, K.E and Christman, M.C. \emph{In preparation.} Restricted adaptive cluster sampling.
#' @export

createPopulation <- function(x_start, x_end, y_start, y_end) {
  data.frame(expand.grid(
    x = 	x_start:x_end,
    y = 	y_start:y_end
  )) # %>%
  # mutate(unitID = 1:length(x))
}
