
# Overview

## Description

A small package used to block records for data deduplication and record
linkage (entity resolution) based on [approximate nearest neighbours
algorithms (ANN)](https://en.wikipedia.org/wiki/Nearest_neighbor_search)
and graphs (via `igraph`).

Currently supports:

- `RcppHNSW`,
- `RcppAnnoy`,
- `mlpack` (`mlpack::lsh` and `mlpack::knn`).

## Funding

Work on this package is supported by the the National Science Centre,
OPUS 22 grant no. 2020/39/B/HS4/00941.

## Installation

You can install the development version of `blocking` from GitHub with:

``` r
# install.packages("remotes") # uncomment if needed
remotes::install_github("ncn-foreigners/blocking")
```
