#' Title My Binomial Distribution
#'
#' @param iter Number of iterations
#' @param n Number of trials
#' @param p Probability of trial success
#'
#' @return An algorithm that makes a barplot of expected values from a binomial distribution
#' @export
#'
#' @examples
#' \dontrun{mybin(100,10,.5)}

## sampling function
# iter = iterations, n=sample size
# set default values
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
