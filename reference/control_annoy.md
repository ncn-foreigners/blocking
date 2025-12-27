# Controls for the Annoy algorithm

Controls for Annoy algorithm used in the package (see
[RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html)
for details).

## Usage

``` r
control_annoy(n_trees = 250, build_on_disk = FALSE, ...)
```

## Arguments

- n_trees:

  An integer specifying the number of trees to build in the Annoy index.

- build_on_disk:

  A logical value indicating whether to build the Annoy index on disk
  instead of in memory.

- ...:

  Additional arguments.

## Value

Returns a list with parameters.
