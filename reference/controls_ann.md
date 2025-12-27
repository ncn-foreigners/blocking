# Controls for approximate nearest neighbours algorithms

Controls for ANN algorithms used in the package.

## Usage

``` r
controls_ann(
  sparse = FALSE,
  k_search = 30,
  nnd = control_nnd(),
  hnsw = control_hnsw(),
  lsh = control_lsh(),
  kd = control_kd(),
  annoy = control_annoy()
)
```

## Arguments

- sparse:

  whether sparse data should be used as an input for algorithms,

- k_search:

  number of neighbours to search,

- nnd:

  parameters for
  [rnnd_build](https://jlmelville.github.io/rnndescent/reference/rnnd_build.html)
  and
  [rnnd_query](https://jlmelville.github.io/rnndescent/reference/rnnd_query.html)
  (should be inside
  [control_nnd](https://ncn-foreigners.ue.poznan.pl/blocking/reference/control_nnd.md)
  function),

- hnsw:

  parameters for
  [hnsw_build](https://rdrr.io/pkg/RcppHNSW/man/hnsw_build.html) and
  [hnsw_search](https://rdrr.io/pkg/RcppHNSW/man/hnsw_search.html)
  (should be inside
  [control_hnsw](https://ncn-foreigners.ue.poznan.pl/blocking/reference/control_hnsw.md)
  function),

- lsh:

  parameters for [lsh](https://rdrr.io/pkg/mlpack/man/lsh.html) function
  (should be inside
  [control_lsh](https://ncn-foreigners.ue.poznan.pl/blocking/reference/control_lsh.md)
  function),

- kd:

  kd parameters for [knn](https://rdrr.io/pkg/mlpack/man/knn.html)
  function (should be inside
  [control_kd](https://ncn-foreigners.ue.poznan.pl/blocking/reference/control_kd.md)
  function),

- annoy:

  parameters for
  [RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html)
  package (should be inside
  [control_annoy](https://ncn-foreigners.ue.poznan.pl/blocking/reference/control_annoy.md)
  function).

## Value

Returns a list with parameters.

## Author

Maciej Beręsewicz
