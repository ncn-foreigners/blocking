# version 1.1.0

+ Added a DuckDB backend based on the `vss` extension.
+ Added `duckdb_setup_vss()` for one-time DuckDB `vss` installation,
update and `HNSW_INDEX_JOIN` diagnostics outside `blocking()`.
+ Added support for DuckDB runs from character vectors or precomputed
dense / sparse matrices.
+ Added DuckDB `join_mode` controls and verbose reporting of whether
`HNSW_INDEX_JOIN` is available and used.
+ DuckDB extensions are now expected to be installed before calling
`blocking()`, with clearer setup messages when `vss` is missing.
+ Fixed several edge cases in blocking and evaluation, including
singleton deduplication, empty feature overlap, and `eval_reclin()`
handling of unmatched records.

# version 1.0.0

+ Added support for word embeddings.
+ Updated controls for the ANN algorithms.
+ Updated evaluation metrics.
+ Added 4 data sets.
+ Added 2 new vignettes.
+ Added estimators for FPR and FNR due to blocking in record linkage,
as proposed by Dasylva and Goussanou (2021).
+ Updated examples and documentation.

# version 0.1.0

+ Supports the following packages: `RcppHNSW`, `mlpack` and `RcppAnnoy`.
+ Supports blocking for deduplication and record linkage.
+ Metrics when true blocking is known based on `igraph::compare`.
+ Testing with the `tinytest` package.
+ Initial support for the `reclin2` package.
+ Class `blocking` introduced.
+ S3method for printing.
+ First vignette added.
+ Evaluation with standard metrics (recall, fpr, etc.) added, works with vector for deduplication.
+ Added saving index for hnsw and annoy.
+ `rnndescend` support added.
