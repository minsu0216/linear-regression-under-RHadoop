\name{summary.rmrlm}
\alias{rmrlm}
\title{Summarizing Linear Model Fits in RHadoop}
\description{ summary method for class "rmrlm". }
\usage{
## S3 method for class 'rmrlm'
summary(object, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
\item{object}{ an object of class "rmrlm", a result of a call to rmrlm }
\item{digits}{ the number of significant digits to use when printing. }
}
\value{
The function summary.rmrlm computes and returns a list of summary statistics of the fitted linear model given in object,
\item{coefficients}{ a p x 4 matrix with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value. Aliased coefficients are omitted. }
\item{r.squared}{ R^2, the ¡®fraction of variance explained by the model¡¯ }
\item{adj.r.squared}{ the above R^2 statistic ¡®adjusted¡¯, penalizing for higher p. }
\item{fstatistic}{ (for models including non-intercept terms) a 3-vector with the value of the F-statistic with its numerator and denominator degrees of freedom. }
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
