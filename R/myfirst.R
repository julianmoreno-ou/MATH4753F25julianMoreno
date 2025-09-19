#' my first function
#'
#' @param x A numeric Vector.
#'
#' @returns A list with components 'x' and 'y', where 'y' is the squre of 'x'
#'
#' @export
#'
#' @examples
#' myfirstfunction(1:10)


myfirstfunction <- function(x) {
  y <- x^2
  plot(y - x)
  list(x = x, y = y)
}
