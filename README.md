
<!-- badges: start -->

[![R-CMD-check](https://github.com/ncn-foreigners/blocking/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ncn-foreigners/blocking/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/ncn-foreigners/blocking/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ncn-foreigners/blocking/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ncn-foreigners/blocking/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ncn-foreigners/blocking?branch=main)
<!-- badges: end -->

# Overview

## Description

This R package is designed to block records for data deduplication and
record linkage (also known as entity resolution) using [approximate
nearest neighbours algorithms
(ANN)](https://en.wikipedia.org/wiki/Nearest_neighbor_search) and graphs
(via the `igraph` package).

It supports the following R packages that bind to specific ANN
algorithms:

- [rnndescent](https://cran.r-project.org/package=rnndescent) (default,
  very powerful, supports sparse matrices),
- [RcppHNSW](https://cran.r-project.org/package=RcppHNSW) (powerful but
  does not support sparse matrices),
- [RcppAnnoy](https://cran.r-project.org/package=RcppAnnoy),
- [mlpack](https://cran.r-project.org/package=mlpack) (see `mlpack::lsh`
  and `mlpack::knn`).

The package can be used with the
[reclin2](https://cran.r-project.org/package=reclin2) package via the
`blocking::pair_ann` function.

## Installation

Install the GitHub blocking package with:

``` r
# install.packages("remotes") # uncomment if needed
remotes::install_github("ncn-foreigners/blocking")
```

## Basic usage

Load packages for the examples

``` r
library(blocking)
library(reclin2)
#> Ładowanie wymaganego pakietu: data.table
#> data.table 1.17.0 using 6 threads (see ?getDTthreads).  Latest news: r-datatable.com
```

Generate simple data with three groups (`df_example`) and reference data
(`df_base`).

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
df_base <- data.frame(txt = c("montypython", "kowalskijan", "other"))

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

df_base
#>           txt
#> 1 montypython
#> 2 kowalskijan
#> 3       other
```

Deduplication using the `blocking` function. Output contains
information:

- the method used (where `nnd` which refers to the NN descent
  algorithm),
- number of blocks created (here 2 blocks),
- number of columns used for blocking, i.e. how many shingles were
  created by `text2vec` package (here 28),
- reduction ratio, i.e. how large is the reduction of comparison pairs
  (here 0.5714 which means blocking reduces comparison by over 57%).

``` r
blocking_result <- blocking(x = df_example$txt)
blocking_result
#> ========================================================
#> Blocking based on the nnd method.
#> Number of blocks: 2.
#> Number of columns used for blocking: 28.
#> Reduction ratio: 0.5714.
#> ========================================================
#> Distribution of the size of the blocks:
#> 4 
#> 2
```

Table with blocking results contains:

- row numbers from the original data,
- block number (integers),
- distance (from the ANN algorithm).

``` r
blocking_result$result
#>        x     y block       dist
#>    <int> <int> <num>      <num>
#> 1:     1     2     1 0.10000002
#> 2:     2     3     1 0.14188367
#> 3:     2     4     1 0.28286284
#> 4:     5     6     2 0.08333331
#> 5:     5     7     2 0.13397455
#> 6:     5     8     2 0.27831215
```

Deduplication using the `pair_ann` function for integration with the
`reclin2` package. Use the pipeline with the `reclin2` package.

``` r
pair_ann(x = df_example, on = "txt") |>
  compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
  score_simple("score", on = "txt") |>
  select_threshold("threshold", score = "score", threshold = 0.55) |>
  link(selection = "threshold")
#>   Total number of pairs: 8 pairs
#> 
#> Key: <.y>
#>       .y    .x       txt.x           txt.y
#>    <int> <int>      <char>          <char>
#> 1:     2     1 jankowalski     kowalskijan
#> 2:     3     1 jankowalski    kowalskimjan
#> 3:     3     2 kowalskijan    kowalskimjan
#> 4:     4     1 jankowalski        kowaljan
#> 5:     4     2 kowalskijan        kowaljan
#> 6:     6     5 montypython     pythonmonty
#> 7:     7     5 montypython cyrkmontypython
#> 8:     8     5 montypython           monty
```

Linking records using the same function where `df_base` is the
“register” and `df_example` is the reference (data).

``` r
pair_ann(x = df_base, y = df_example, on = "txt", deduplication = FALSE) |>
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

See section `Data Integration (Statistical Matching and Record Linkage)`
in [the Official Statistics Task
View](https://CRAN.R-project.org/view=OfficialStatistics).

Packages that allow blocking:

- [klsh](https://CRAN.R-project.org/package=klsh) – k-means locality
  sensitive hashing,
- [reclin2](https://CRAN.R-project.org/package=reclin2) –
  `pair_blocking`, `pari_minsim` functions,
- [fastLink](https://CRAN.R-project.org/package=fastLink) – `blockData`
  function.

Other:

- [clevr](https://CRAN.R-project.org/package=clevr) – evaluation of
  clustering, helper functions.
- [exchanger](https://github.com/cleanzr/exchanger) – bayesian Entity
  Resolution with Exchangeable Random Partition Priors

## Funding

Work on this package is supported by the National Science Centre, OPUS
20 grant no. 2020/39/B/HS4/00941.
