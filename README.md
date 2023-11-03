
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

The package also supports integration with
[reclin2](https://cran.r-project.org/package=reclin2) package via
`blocking::pair_ann` function.

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

Load packages for the examples

``` r
library(blocking)
library(reclin2)
#> Loading required package: data.table
#> 
#> Attaching package: 'reclin2'
#> The following object is masked from 'package:base':
#> 
#>     identical
```

Generate simple data with two groups.

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
df_base <- data.frame(txt = c("montypython", "kowalskijan"))

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

Deduplication using blocking

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

Deduplication followed by the `reclin2` package

``` r
pair_ann(x = df_example, on = "txt") |>
  compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
  score_simple("score", on = "txt") |>
  select_threshold("threshold", score = "score", threshold = 0.55) |>
  link(selection = "threshold")
#>   Total number of pairs: 10 pairs
#> 
#> Key: <.y>
#>        .y    .x       txt.x           txt.y
#>     <int> <int>      <char>          <char>
#>  1:     2     1 jankowalski     kowalskijan
#>  2:     3     1 jankowalski    kowalskimjan
#>  3:     3     2 kowalskijan    kowalskimjan
#>  4:     4     1 jankowalski        kowaljan
#>  5:     4     2 kowalskijan        kowaljan
#>  6:     6     5 montypython     pythonmonty
#>  7:     7     5 montypython cyrkmontypython
#>  8:     7     6 pythonmonty cyrkmontypython
#>  9:     8     5 montypython           monty
#> 10:     8     6 pythonmonty           monty
```

Record linkage

``` r
pair_ann(y = df_example, x = df_base, on = "txt", deduplication = FALSE) |>
  compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
  score_simple("score", on = "txt") |>
  select_threshold("threshold", score = "score", threshold = 0.55) |>
  link(selection = "threshold")
#>   Total number of pairs: 8 pairs
#> 
#> Key: <.y>
#>       .y    .x       txt.x           txt.y
#>    <int> <int>      <char>          <char>
#> 1:     1     2 kowalskijan     jankowalski
#> 2:     2     2 kowalskijan     kowalskijan
#> 3:     3     2 kowalskijan    kowalskimjan
#> 4:     4     2 kowalskijan        kowaljan
#> 5:     5     1 montypython     montypython
#> 6:     6     1 montypython     pythonmonty
#> 7:     7     1 montypython cyrkmontypython
#> 8:     8     1 montypython           monty
```

## See also

- [reclin2](https://CRAN.R-project.org/package=reclin2)
- [klsh](https://CRAN.R-project.org/package=klsh)
