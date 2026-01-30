# Block records based on character vectors

Function creates shingles (strings with 2 characters, default) or
vectors using a given model (e.g., GloVe), applies approximate nearest
neighbour (ANN) algorithms via the
[rnndescent](https://jlmelville.github.io/rnndescent/reference/rnndescent-package.html),
[RcppHNSW](https://rdrr.io/pkg/RcppHNSW/man/RcppHnsw-package.html),
[RcppAnnoy](https://rdrr.io/pkg/RcppAnnoy/man/RcppAnnoy-package.html)
and [mlpack](https://rdrr.io/pkg/mlpack/man/mlpack.html) packages, and
creates blocks using graphs via
[igraph](https://r.igraph.org/reference/aaa-igraph-package.html).

## Usage

``` r
blocking(
  x,
  y = NULL,
  representation = c("shingles", "vectors"),
  model,
  deduplication = TRUE,
  on = NULL,
  on_blocking = NULL,
  ann = c("nnd", "hnsw", "annoy", "lsh", "kd"),
  distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
  ann_write = NULL,
  ann_colnames = NULL,
  true_blocks = NULL,
  verbose = c(0, 1, 2),
  graph = FALSE,
  seed = 2023,
  n_threads = 1,
  control_txt = controls_txt(),
  control_ann = controls_ann()
)
```

## Arguments

- x:

  reference data (a character vector or a matrix),

- y:

  query data (a character vector or a matrix), if not provided NULL by
  default and thus deduplication is performed,

- representation:

  method of representing input data (possible
  `c("shingles", "vectors")`; default `"shingles"`),

- model:

  a matrix containing word embeddings (e.g., GloVe), required only when
  `representation = "vectors"`,

- deduplication:

  whether deduplication should be applied (default TRUE as y is set to
  NULL),

- on:

  variables for ANN search (currently not supported),

- on_blocking:

  variables for blocking records before ANN search (currently not
  supported),

- ann:

  algorithm to be used for searching for ann (possible,
  `c("nnd", "hnsw", "annoy", "lsh", "kd")`, default `"nnd"` which
  corresponds to nearest neighbour descent method),

- distance:

  distance metric (default `cosine`, more options are possible see
  details),

- ann_write:

  writing an index to file. Two files will be created: 1) an index, 2)
  and text file with column names,

- ann_colnames:

  file with column names if `x` or `y` are indices saved on the disk
  (currently not supported),

- true_blocks:

  `data.frame` with true blocks to calculate evaluation metrics
  (standard metrics based on confusion matrix are returned). This
  `data.frame` must contain three columns: `x`, `y`, and `block`.

- verbose:

  whether log should be provided (0 = none, 1 = main, 2 = ANN algorithm
  verbose used),

- graph:

  whether a graph should be returned (default FALSE),

- seed:

  seed for the algorithms (for reproducibility),

- n_threads:

  number of threads used for the ANN algorithms and adding data for
  index and query,

- control_txt:

  list of controls for text data (passed only to
  [itoken_parallel](https://rdrr.io/pkg/text2vec/man/itoken.html) or
  [itoken](https://rdrr.io/pkg/text2vec/man/itoken.html)), used only
  when `representation = "shingles"`,

- control_ann:

  list of controls for the ANN algorithms.

## Value

Returns a list containing:  

- `result` – `data.table` with indices (rows) of x, y, block and
  distance between points

- `method` – name of the ANN algorithm used,

- `deduplication` – information whether deduplication was applied,

- `representation` – information whether shingles or vectors were used,

- `metrics` – metrics for quality assessment, if `true_blocks` is
  provided,

- `confusion` – confusion matrix, if `true_blocks` is provided,

- `colnames` – variable names (colnames) used for search,

- `graph` – `igraph` class object.

## Author

Maciej Beręsewicz, Adam Struzik

## Examples

``` r
## an example using RcppHNSW

df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
"kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))

result <- blocking(x = df_example$txt,
                   ann = "hnsw",
                   control_ann = controls_ann(hnsw = control_hnsw(M = 5, ef_c = 10, ef_s = 10)))

result
#> ========================================================
#> Blocking based on the hnsw method.
#> Number of blocks: 2.
#> Number of shingles created for blocking: 28.
#> Reduction ratio: 0.5714.
#> ========================================================
#> Distribution of the size of the blocks:
#> 4 
#> 2 

## an example using GloVe and RcppAnnoy
if (FALSE) { # \dontrun{
old <- getOption("timeout")
options(timeout = 500)
utils::download.file("https://nlp.stanford.edu/data/glove.6B.zip", destfile = "glove.6B.zip")
utils::unzip("glove.6B.zip")

glove_6B_50d <- readr::read_table("glove.6B.50d.txt",
                                  col_names = FALSE,
                                  show_col_types = FALSE)
data.table::setDT(glove_6B_50d)

glove_vectors <- glove_6B_50d[,-1]
glove_vectors <- as.matrix(glove_vectors)
rownames(glove_vectors) <- glove_6B_50d$X1

## spaces between words are required
df_example_spaces <- data.frame(txt = c("jan kowalski", "kowalski jan", "kowalskim jan",
"kowal jan", "monty python", "python monty", "cyrk monty python", "monty"))

result_annoy <- blocking(x = df_example_spaces$txt,
                         ann = "annoy",
                         representation = "vectors",
                         model = glove_vectors)

result_annoy

options(timeout = old)
} # }
```
