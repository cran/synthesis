% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_gen_Class.R
\name{data.gen.blobs}
\alias{data.gen.blobs}
\title{Gaussian Blobs}
\usage{
data.gen.blobs(
  nobs = 100,
  features = 2,
  centers = 3,
  sd = 1,
  bbox = c(-10, 10),
  do.plot = TRUE
)
}
\arguments{
\item{nobs}{The data length to be generated.}

\item{features}{Features of dataset.}

\item{centers}{Either the number of centers, or a matrix of the chosen centers.}

\item{sd}{The level of Gaussian noise, default 1.}

\item{bbox}{The bounding box of the dataset.}

\item{do.plot}{Logical value. If TRUE (default value), a plot of the generated Blobs is shown.}
}
\value{
A list of two variables, x and classes.
}
\description{
Gaussian Blobs
}
\details{
This function generates a matrix of features creating multiclass datasets
by allocating each class one or more normally-distributed clusters of points.
It can control both centers and standard deviations of each cluster.
For example, we want to generate a dataset of weight and height (two features) of 500 people (data length),
including three groups, baby, children, and adult. Centers are the average weight and height for each group,
assuming both weight and height are normally distributed (i.e. follow Gaussian distribution).
The standard deviation (sd) is the sd of the Gaussian distribution
while the bounding box (bbox) is the range for each generated cluster center when only the number of centers is given.
}
\examples{
Blobs=data.gen.blobs(nobs=1000, features=2, centers=3, sd=1, bbox=c(-10,10), do.plot=TRUE)
}
\references{
Amos Elberg (2018). clusteringdatasets: Datasets useful for testing clustering algorithms. R package version 0.1.1. https://github.com/elbamos/clusteringdatasets
}
