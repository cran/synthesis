% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_gen_BM.R
\name{data.gen.gbm}
\alias{data.gen.gbm}
\title{Generate a time series of geometric Brownian motion.}
\usage{
data.gen.gbm(
  x0 = 10,
  w0 = 0,
  mu = 1,
  sigma = 0.5,
  time = seq(0, by = 0.01, length.out = 101),
  do.plot = TRUE
)
}
\arguments{
\item{x0}{the start value of x, with the default value 10}

\item{w0}{the start value of w, with the default value 0}

\item{mu}{the interest/drifting rate, with the default value 1.}

\item{sigma}{the diffusion coefficient, with the default value 0.5.}

\item{time}{the temporal interval at which the system will be generated. Default seq(0,by=0.01,len=101).}

\item{do.plot}{a logical value. If TRUE (default value), a plot of the generated system is shown.}
}
\value{
A ts object.
}
\description{
This function generates a a time series of one dimension geometric Brownian motion.
}
\examples{
set.seed(123)
x <- data.gen.gbm()
}
\references{
Yanping Chen, http://cos.name/wp-content/uploads/2008/12/stochastic-differential-equation-with-r.pdf
}
