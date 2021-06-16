
# MLClusteringAlgorithms

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/alyssajs/MLClusteringAlgorithmssvg?branch=master)](https://travis-ci.com/alyssajs/MLClusteringAlgorithms)
<!-- badges: end -->

This package includes two clustering algorithms (hierarchical and
spectral clustering) and associated helper
functions.

## Installation

You can install the released version of MLClusteringAlgorithms by using the command
remotes::install_github("alyssajs/MLClusteringAlgorithms")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MLClusteringAlgorithms)
## basic example code
HCLUST(iris[,1:4], 3)

set.seed(1)
halfcircle <- function(r, center = c(0, 0), class, sign, N=150, noise=0.5) {
angle <- runif(N, 0, pi)
rad <- rnorm(N, r, noise)
data.table(
V1 = rad * cos(angle) + center[1],
V2 = sign * rad * sin(angle) + center[2],
)
}
X.dt <- rbind(
halfcircle(4, c(0, 0), 1, 1),
halfcircle(4, c(4, 2), 2, -1))
result <- SPECC(X.dt, 2, 5, 5)
```

