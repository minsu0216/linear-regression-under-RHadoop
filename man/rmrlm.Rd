\name{rmrlm}
\alias{rmrlm}
\title{Linear Regression in bigdata.}
\description{Linear Regression in RHadoop System.}
\usage{
rmrlm(inputfile, input.format = "native", formula, xlev = NULL, combine = FALSE, method = "qr")
}
\arguments{
\item{inputfile}{ A valid path to local data or a big.data.object }
\item{input.format}{ List, If you analyze in the hadoop data, you have to give the format of data. }
\item{formula}{	an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. }
\item{xlev}{ if you have categorical variables, you must list xlev, else you don't have to do anything. }
\item{combine}{ logical; TRUE or FALSE(default) }
\item{method}{ the method to be used. "qr" is using QR decomposition and "ne" is using normal equation method. }
\item{...}{Not used. }
}
\value{
rmrlm returns coefficients vector. If you use summary(.rmrlm), you can see the details.
}
\examples{
#-------Load the packages-------#
library(rmrlm)
library(rhdfs)
library(rmr2)

#-------Prepare the rhadoop environment-------#
hdfs.init()
rmr.options(backend = "hadoop")

#-------Run the function-------#
taxi_formula <- trip_distance ~ fare_amount + total_amount
fit <- rmrlm(inputfile = to.dfs(taxi), input.format = "native", formula = taxi_formula)
summary(fit)
}
\keyword{rmrlm}
