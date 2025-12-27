# Controls for the k-d tree algorithm

Controls for KD algorithm used in the package (see
[knn](https://rdrr.io/pkg/mlpack/man/knn.html) for details).

## Usage

``` r
control_kd(
  algorithm = "dual_tree",
  epsilon = 0,
  leaf_size = 20,
  random_basis = FALSE,
  rho = 0.7,
  tau = 0,
  tree_type = "kd",
  ...
)
```

## Arguments

- algorithm:

  Type of neighbor search: `'naive'`, `'single_tree'`, `'dual_tree'`,
  `'greedy'`.

- epsilon:

  If specified, will do approximate nearest neighbor search with given
  relative error.

- leaf_size:

  Leaf size for tree building (used for kd-trees, vp trees, random
  projection trees, UB trees, R trees, R\* trees, X trees, Hilbert R
  trees, R+ trees, R++ trees, spill trees, and octrees).

- random_basis:

  Before tree-building, project the data onto a random orthogonal basis.

- rho:

  Balance threshold (only valid for spill trees).

- tau:

  Overlapping size (only valid for spill trees).

- tree_type:

  Type of tree to use: `'kd'`, `'vp'`, `'rp'`, `'max-rp'`, `'ub'`,
  `'cover'`, `'r'`, `'r-star'`, `'x'`, `'ball'`, `'hilbert-r'`,
  `'r-plus'`, `'r-plus-plus'`, `'spill'`, `'oct'`.

- ...:

  Additional arguments.

## Value

Returns a list with parameters.
