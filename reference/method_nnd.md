# An internal function to use the NN descent algorithm via the [rnndescent](https://jlmelville.github.io/rnndescent/reference/rnndescent-package.html) package.

See details of
[rnnd_build](https://jlmelville.github.io/rnndescent/reference/rnnd_build.html)
and
[rnnd_query](https://jlmelville.github.io/rnndescent/reference/rnnd_query.html).

## Usage

``` r
method_nnd(x, y, k, distance, deduplication, verbose, n_threads, control, seed)
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

- deduplication:

  whether the deduplication is applied,

- verbose:

  if TRUE, log messages to the console,

- n_threads:

  maximum number of threads to use,

- control:

  controls for the NN descent algorithm.

## Author

Maciej Beręsewicz
