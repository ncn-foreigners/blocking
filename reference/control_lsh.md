# Controls for the LSH algorithm

Controls for LSH algorithm used in the package (see
[lsh](https://rdrr.io/pkg/mlpack/man/lsh.html) for details).

## Usage

``` r
control_lsh(
  bucket_size = 10,
  hash_width = 6,
  num_probes = 5,
  projections = 10,
  tables = 30,
  ...
)
```

## Arguments

- bucket_size:

  The size of a bucket in the second level hash.

- hash_width:

  The hash width for the first-level hashing in the LSH preprocessing.

- num_probes:

  Number of additional probes for multiprobe LSH.

- projections:

  The number of hash functions for each table.

- tables:

  The number of hash tables to be used.

- ...:

  Additional arguments.

## Value

Returns a list with parameters.
