#' Title MyNCurve
#'
#' @param mu Mean of distribution
#' @param sigma Std. Deviation of Distribution
#' @param a Requested value
#'
#' @return
#' @export
#'
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma), lwd = 3)
  xcurve = seq(-3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(-3*sigma, xcurve, a), c(0, ycurve, 0), col = "Red")
  prob = round((pnorm(a,mu,sigma)), 4)
  text(x = a, y = 0.02, paste("Area = ", prob, sep = ""))
}
