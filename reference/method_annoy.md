# An internal function to use Annoy algorithm via the [RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html) package.

See details of the
[RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html)
package.

## Usage

``` r
method_annoy(x, y, k, distance, verbose, path, seed, control)
```

## Arguments

- x:

  deduplication or reference data,

- y:

  query data,

- k:

  number of neighbours to return,

- distance:

  distance metric,

- verbose:

  if TRUE, log messages to the console,

- path:

  path to write the index,

- seed:

  seed for the pseudo-random numbers algorithm,

- control:

  controls for `new` or `build` methods for
  [RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html).

## Author

Maciej Beręsewicz
