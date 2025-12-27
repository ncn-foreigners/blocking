# An internal function to use the LSH and KD-tree algorithm via the [mlpack](https://rdrr.io/pkg/mlpack/man/mlpack.html) package.

See details of [lsh](https://rdrr.io/pkg/mlpack/man/lsh.html) and
[knn](https://rdrr.io/pkg/mlpack/man/knn.html).

## Usage

``` r
method_mlpack(x, y, algo = c("lsh", "kd"), k, verbose, seed, path, control)
```

## Arguments

- x:

  deduplication or reference data,

- y:

  query data,

- algo:

  which algorithm should be used: `lsh` or `kd`,

- k:

  number of neighbours to return,

- verbose:

  if TRUE, log messages to the console,

- seed:

  seed for the pseudo-random numbers algorithm,

- path:

  path to write the index,

- control:

  controls for the `lsh` or `kd` algorithms.

## Author

Maciej Beręsewicz
