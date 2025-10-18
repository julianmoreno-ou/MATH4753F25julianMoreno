#' Title
#'
#' @param x Number of people in a random sample
#'
#' @returns Probabilit(y/ies) of 2 or more people in sample(s) sharing same birthday
#' @export
#'
#' @examples
#' birthday(20:24)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))

}
