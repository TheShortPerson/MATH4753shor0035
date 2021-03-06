% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{MyBoot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{the desired sample}

\item{fun}{desired parameter -- mean, median, sd, var, iqr, etc}

\item{alpha}{confidence level}

\item{cx}{size of text characters}
}
\value{
a histogram depicting the confidence interval specified
}
\description{
MyBoot2
}
\examples{
\dontrun{myboot2(iter = 10000, x = sam, fun = "mean", alpha = 0.05)}
}
