mybin=function(iter=100,n=10, p=0.5){
  # create an empty matrix with n rows and iter columns
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c() # success counter
  for( i in 1:iter){
    # for each iterations, add sample to row
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    # return sum of sample
    succ[i]=sum(sam.mat[,i])
  }
  #creates table of successes
  succ.tab=table(factor(succ,levels=0:n))
  # make barplot of simulation
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
