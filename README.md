# rmrlm
package implementing linear regression algorithms using Normal Equation and QR decomposition in RHadoop

## Installation
``` r
# Install development version from GitHub
devtools::install_github("minsu0216/rmrlm")
```

## Usage
``` r
rmrlm(inputfile, input.format = "native", formula, xlev = NULL, combine = FALSE, method = "qr")
```

## Arguments
inputfile A valid path to local data or a big.data.object

input.format  List, If you analyze in the hadoop data, you have to give the format of data.

formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.

xlev  if you have categorical variables, you must list xlev, else you don't have to do anything.

combine logical; TRUE or FALSE(default)

method  the method to be used. "qr" is using QR decomposition and "ne" is using normal equation method.

... Not used.

## Values
rmrlm returns coefficients vector. If you use summary(.rmrlm), you can see the details.

## Examples
``` r
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
```
