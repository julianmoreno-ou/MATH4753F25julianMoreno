#' Title
#'
#' @param N Number of seats available, default is 400 seats
#' @param gamma Probability of overbooking occuring, default is 2\%
#' @param p Probability of any given person who bought a ticket showing, default is 95\%
#'
#'@importFrom stats qbinom pbinom pnorm optimize
#'@importFrom graphics curve abline
#'
#' @returns A list with the optimal number tickets sold where nd is the discrete solution and nd is the continuous approximation solution. Two plots of the objective functions used to obtain nd and nc also shown. Input variables also returned
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets = function(N=400,gamma=0.02,p=0.95){
  # Find how many people we're taking, using discrete method
  nd <- list()
  possibs <- list()
  n_upper = N
  # If no nd found, increase the range to look further
  while(length(nd) == 0){
    n_upper = n_upper + ((1-p)*N) + 1
    possibs = N:n_upper # Array of possible people who may show up
    results = qbinom(1-gamma,possibs,p)
    length(results)
    nd = ((possibs)[which(results == N)])
  }
  # we should have our nd here

  # Plot Discrete objective function with intercept of solution
  layout(matrix(c(1,2),nrow=2,ncol=1))
  plot(possibs,1 - gamma - pbinom(N,possibs,p),type="b",ylab="Objective",xlab="Tickets Sold",main = paste("Objective vs. n to find optimal tickets sold\n(",nd,"), gamma=",gamma,", N=",N," discrete"))
  abline(h=0,v=nd,col='red')


  # Define the continuous approximation objective function
  norm_obj <- function(x){
    abs(pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))-1+gamma)
  }

  # Use our n_upper we found earlier to declare our interval
  norm_intv = c(N,n_upper)
  # Find where nc is minimum - it should only have one minimum of 0
  nc = optimize(norm_obj,interval = norm_intv)$minimum

  # Plot Continuous approximation objective function with intercepts of solution
  curve(norm_obj, xlim = norm_intv,main=paste("Objective vs. n to find optimal tickets sold\n( ",nc," ), gamma=",gamma,", N=",N," continuous approx"),xlab = "Tickets Sold",ylab="Objective")
  abline(h=0,v=nc,col='red')

  return(list(nd=nd,nc=nc,N=N,p=p,gamma=gamma))
}
