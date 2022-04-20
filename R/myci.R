#' @title MyCI
#'
#' @param x sample data
#'
#' @return A 95% confidence interval for the mean of the population being sampled
#' @export
#'
#' @examples
#' \dontrun{myci(x = rnorm(30, mean = 10, sd = 12))}
myci <- function(x){
  n = length(x)
  t = qt(0.975, (n - 1))
  ci = c()
  ci[1] = mean(x) - t*sd(x) / sqrt(n)
  ci[2] = mean(x) + t*sd(x) / sqrt(n)
  ci
}
