
# Overview

## Description

A small package used to block records for data deduplication and record
linkage (entity resolution) based on [approximate nearest neighbours
algorithms (ANN)](https://en.wikipedia.org/wiki/Nearest_neighbor_search)
and graphs (via `igraph`).

Currently supports the following R packages that binds to specific ANN
algorithms

- [RcppHNSW](https://cran.r-project.org/package=RcppHNSW),
- [RcppAnnoy](https://cran.r-project.org/package=RcppAnnoy),
- [mlpack](https://cran.r-project.org/package=RcppAnnoy) (see
  `mlpack::lsh` and `mlpack::knn`).

## Funding

Work on this package is supported by the the National Science Centre,
OPUS 22 grant no. 2020/39/B/HS4/00941.

## Installation

You can install the development version of `blocking` from GitHub with:

``` r
# install.packages("remotes") # uncomment if needed
remotes::install_github("ncn-foreigners/blocking")
```

## Basic usage

``` r
library(blocking)
```

``` r
df_example <- data.frame(txt = c(
  "jankowalski",
  "kowalskijan",
  "kowalskimjan",
  "kowaljan",
  "montypython",
  "pythonmonty",
  "cyrkmontypython",
  "monty"
))
df_example
#>               txt
#> 1     jankowalski
#> 2     kowalskijan
#> 3    kowalskimjan
#> 4        kowaljan
#> 5     montypython
#> 6     pythonmonty
#> 7 cyrkmontypython
#> 8           monty
```

``` r
blocking_result <- blocking(x = df_example$txt)
#> 'as(<dgTMatrix>, "dgCMatrix")' is deprecated.
#> Use 'as(., "CsparseMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").
## data frame with indices and block 
blocking_result$result
#>   x y block
#> 1 2 1     1
#> 2 1 2     1
#> 3 2 3     1
#> 4 2 4     1
#> 5 6 5     2
#> 6 5 6     2
#> 7 5 7     2
#> 8 5 8     2
```

## See also

- [reclin2](https://CRAN.R-project.org/package=reclin2)
- [klsh](https://CRAN.R-project.org/package=klsh)
