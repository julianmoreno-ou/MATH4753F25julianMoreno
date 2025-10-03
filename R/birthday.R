#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))

}
