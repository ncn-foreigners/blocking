# An internal function to use HNSW algorithm via the RcppHNSW package.

See details of
[hnsw_build](https://rdrr.io/pkg/RcppHNSW/man/hnsw_build.html) and
[hnsw_search](https://rdrr.io/pkg/RcppHNSW/man/hnsw_search.html).

## Usage

``` r
method_hnsw(x, y, k, distance, verbose, n_threads, path, control, seed)
```

## Arguments

- x:

  deduplication or reference data,

- y:

  query data,

- k:

  number of neighbours to return,

- distance:

  type of distance to calculate,

- verbose:

  if TRUE, log messages to the console,

- n_threads:

  Maximum number of threads to use,

- path:

  path to write the index,

- control:

  controls for the HNSW algorithm.

## Author

Maciej Beręsewicz
