# Controls for the HNSW algorithm

Controls for HNSW algorithm used in the package (see
[`RcppHNSW::hnsw_build()`](https://rdrr.io/pkg/RcppHNSW/man/hnsw_build.html)
and
[`RcppHNSW::hnsw_search()`](https://rdrr.io/pkg/RcppHNSW/man/hnsw_search.html)
for details).

## Usage

``` r
control_hnsw(M = 25, ef_c = 200, ef_s = 200, grain_size = 1, byrow = TRUE, ...)
```

## Arguments

- M:

  Controls the number of bi-directional links created for each element
  during index construction.

- ef_c:

  Size of the dynamic list used during construction.

- ef_s:

  Size of the dynamic list used during search.

- grain_size:

  Minimum amount of work to do (rows in the dataset to add) per thread.

- byrow:

  If `TRUE` (the default), this indicates that the items in the dataset
  to be indexed are stored in each row. Otherwise, the items are stored
  in the columns of the dataset.

- ...:

  Additional arguments.

## Value

Returns a list with parameters.
