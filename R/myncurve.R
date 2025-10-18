#' Title
#'
#' @param mu The average of the Normal Distribution
#' @param sigma The standard Deviation of the Normal Deviation
#' @param a The value at which the probability calculated from negative infinity goes to
#'
#' @returns An image of the distribution, with the region of the probability shaded along with its associated area rounded to decimal points
#' @export
#'
#' @examples
#' myncurve(500,200,a=300)
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)

  a_strt = mu - 10*sigma
  xcurve = seq(a_strt, a, length = 5000)
  ycurve = dnorm(xcurve,mean = mu, sd = sigma)
  polygon(c(a_strt,xcurve,a),c(0,ycurve,0),col='Red')
  area = round( pnorm(a, mean = mu, sd = sigma) - pnorm(a_strt, mean = mu, sd = sigma), 4)
  text(a, 0.75*max(ycurve), paste("Area = ",area))
}
