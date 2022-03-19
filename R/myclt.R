#' @title MyCLT
#'
#' @param n number of trials per iteration
#' @param iter number of iterations
#' @param a minimum value of sample
#' @param b maximum value of sample
#'
#' @return a histogram with the sample means of repeated sampling
#' @export
#'
#' @examples
#' \dontrun{mycltu(n = 10, iter = 10000, a = 0, b = 10)}
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean) # <- the 2 in this function means that we want to take the means of the columns, not the rows.
  # When mycltu(n = 20, iter = 100000) is called, 100,000 terms are in w, because only the means are stored.
  param=hist(w,plot=FALSE)

  ymax=1.1*max(param$density)
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # The SD term in this function ensures that a normal curve is what is applied to the data.
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
