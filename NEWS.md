# development

# version 1.0.1

+ Fixed CRAN errors
+ [Awesome Official Statistics](https://github.com/SNStatComp/awesome-official-statistics-software) badge added
+ Removed unnecessary dependency on the `RcppAlgos` package

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
