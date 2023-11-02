#' Imports
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom RcppAnnoy AnnoyEuclidean
#' @importFrom RcppAnnoy AnnoyHamming
#' @importFrom RcppAnnoy AnnoyManhattan
#' @importFrom mlpack lsh
#' @importFrom mlpack knn
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph make_clusters
#' @importFrom igraph compare
#' @importFrom methods new
#'
#'
#' @title Main function for blocking records given text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Function that creates shingles (strings with 2 characters), applies approximate nearest neibhour search using
#' \code{RcppHNSW} and creates blocks using \code{igraph}
#'
#' @param x input text or matrix data,
#' @param y input text or matrix data (default NULL),
#' @param deduplication whether deduplication should be applied (default TRUE as y is set to NULL),
#' @param block initial blocking to reduce comparisons (currently not supported),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("hnsw", "lsh", "annoy", "kd")},
#' default \code{"hnsw"}),
#' @param distance distance metric (default \code{cosine}),
#' @param true_blocks matrix with true blocks to calculate evaluation metrics (all metrics from [igraph::compare()] are returned).
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ann algorithms),
#' @param seed seed for the algorithms,
#' @param n_threads number of threads used for the ann,
#' @param control_txt list of controls for text data,
#' @param control_ann list of controls for ann algorithms.
#'
#' @returns Returns a list with containing:\cr
#' \itemize{
#' \item{\code{result} -- \code{data.frame} with indices (rows) of x, y and block}
#' \item{\code{ann} -- name of the ann algorithm used,}
#' \item{\code{metrics} -- metrics, if \code{true_blocks} is provided,}
#' \item{\code{colnames} -- variable names (colnames) used for search.}
#' }
#'
#' @examples
#'
#' ## general example
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))
#'
#' result
#'
#' ## an example with true blocks
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = TRUE,
                     block = NULL,
                     ann = c("hnsw", "lsh", "annoy", "kd"),
                     distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
                     true_blocks = NULL,
                     verbose = c(0, 1, 2),
                     n_threads = 1,
                     seed = 2023,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  ## checks
  stopifnot("Only character or matrix x is supported" = is.character(x) | is.matrix(x))

  if (!is.null(true_blocks)) {
    stopifnot("`true block` should be a data.frame with columns: x, y, block" =
                !is.null(true_blocks) &
                is.data.frame(true_blocks) &
                length(colnames(true_blocks)) == 3,
              all(colnames(true_blocks) == c("x", "y", "block")))
  }

  ## defaults
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "hnsw"
  if (missing(distance)) distance <- "cosine"
  if (!is.null(y)) {
    deduplication <- FALSE
    y_default <- FALSE
  }

  k <- 1L

  if (is.null(y)) {
    y_default <- y
    y <- x
    k <- 2L
  }

  if (is.matrix(x)) {
    l_dtm <- x
    l_dtm_y <- y
  } else {

    if (verbose %in% 1:2) cat("===== creating tokens =====\n")

    ## tokens for x
    l_tokens <- text2vec::itoken_parallel(
      iterable = x,
      tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
      n_chunks = control_txt$n_chunks,
      progressbar = verbose)

    l_voc <- text2vec::create_vocabulary(l_tokens)
    l_vec <- text2vec::vocab_vectorizer(l_voc)
    l_dtm <- text2vec::create_dtm(l_tokens, l_vec)
    l_dtm <- base::as.matrix(l_dtm)

    ## tokens for y (check history to avoid copying)
    if (is.null(y_default)) {
      l_dtm_y <- l_dtm
    } else {
      l_tokens_y <- text2vec::itoken_parallel(
        iterable = y,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)

      l_voc_y <- text2vec::create_vocabulary(l_tokens_y)
      l_vec_y <- text2vec::vocab_vectorizer(l_voc_y)
      l_dtm_y <- text2vec::create_dtm(l_tokens_y, l_vec_y)
      l_dtm_y <- base::as.matrix(l_dtm_y)
    }
  }

  colnames_xy <- intersect(colnames(l_dtm), colnames(l_dtm_y))

  if (verbose %in% 1:2) {
    cat(sprintf("===== starting search (%s, x, y: %d, %d, t: %d) =====\n",
                ann, nrow(l_dtm), nrow(l_dtm_y), length(colnames_xy)))
  }

  ## switch with separate functions for each package?
  if (ann == "hnsw") {

    l_df <- method_hnsw(x = l_dtm[, colnames_xy],
                        y = l_dtm_y[, colnames_xy],
                        deduplication = deduplication,
                        k = k,
                        distance = distance,
                        verbose = if (verbose == 2) TRUE else FALSE,
                        n_threads = n_threads,
                        M = control_ann$hnsw$M,
                        ef_c = control_ann$hnsw$ef_c,
                        ef_s = control_ann$hnsw$ef_s)
  }

  if (ann == "lsh") {

    l_lhs_result <- mlpack::lsh(k = k,
                                query = l_dtm_y[, colnames_xy],
                                reference = l_dtm[, colnames_xy],
                                verbose = if (verbose == 2) TRUE else FALSE,
                                seed = seed,
                                bucket_size = control_ann$lsh$bucket_size,
                                hash_width = control_ann$lsh$hash_width,
                                num_probes = control_ann$lsh$num_probes,
                                projections = control_ann$lsh$projections,
                                tables = control_ann$lsh$tables)

    l_df <- base::data.frame(y = 1:NROW(y),
                             x = l_lhs_result$neighbors[, k] + 1)

  }

  if (ann == "kd") {

    l_knn_result <- mlpack::knn(k = k,
                                query = l_dtm_y[, colnames_xy],
                                reference = l_dtm[, colnames_xy],
                                verbose = if (verbose == 2) TRUE else FALSE,
                                seed = seed,
                                algorithm = control_ann$kd$algorithm,
                                leaf_size = control_ann$kd$leaf_size,
                                tree_type = control_ann$kd$tree_type,
                                eps = control_ann$kd$epsilon,
                                rho = control_ann$kd$rho,
                                tau = control_ann$kd$tau,
                                random_basis = control_ann$kd$random_basis)

    l_df <- base::data.frame(y = 1:NROW(y),
                             x = l_knn_result$neighbors[, k] + 1)

  }

  if (ann == "annoy") {

    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                distance %in% c("euclidean", "manhatan", "hamming", "angular"))

    colnames_xy_n <- length(colnames_xy)
    l_ind <- switch(distance,
                    "euclidean" = methods::new(RcppAnnoy::AnnoyManhattan, colnames_xy_n),
                    "manhatan"  = methods::new(RcppAnnoy::AnnoyManhattan, colnames_xy_n),
                    "hamming"   = methods::new(RcppAnnoy::AnnoyHamming,   colnames_xy_n),
                    "angular"   = methods::new(RcppAnnoy::AnnoyAngular,   colnames_xy_n)
                    )

    l_ind$setSeed(seed)

    if (verbose == 2) l_ind$setVerbose(1)

    ## index
    for (i in 1:nrow(l_dtm)) l_ind$addItem(i - 1, l_dtm[i, colnames_xy])
    l_ind$build(control_ann$annoy$n_trees)
    l_ind_nns <- numeric(length = nrow(l_dtm_y))

    ## query
    for (i in 1:nrow(l_dtm_y)) l_ind_nns[i] <- l_ind$getNNsByVector(l_dtm_y[i, colnames_xy], k)[k]

    l_df <- base::data.frame(y = 1:NROW(y),
                             x = l_ind_nns + 1)

  }

  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  if (deduplication) {
    l_df$query_g <- paste0("q", l_df$y)
    l_df$index_g <- paste0("q", l_df$x)
  } else {
    l_df$query_g <- paste0("q", l_df$y)
    l_df$index_g <- paste0("i", l_df$x)
  }

  ## wybor kolumn powinien zalezec od tego czy jest tylko x czy x, y
  l_gr <- igraph::graph_from_data_frame(l_df[, c("query_g", "index_g")], directed = F)
  l_block <- igraph::components(l_gr, "weak")$membership
  l_df$block <- l_block[names(l_block) %in% l_df$query_g]

  ## if true are given
  if (!is.null(true_blocks)) {
    ## commom part
    eval_blocks <- merge(x = l_df[, c("x", "y", "block")],
                         y = true_blocks,
                         by = c("x", "y"),
                         all = F)

    eval_g1 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)
    eval_g2 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)

    eval_g1_cl <- igraph::make_clusters(eval_g1, membership = eval_blocks$block.x)
    eval_g2_cl <- igraph::make_clusters(eval_g2, membership = eval_blocks$block.y)

    eval_metrics <- base::sapply(c("vi", "nmi", "split.join", "rand", "adjusted.rand"),
                                 igraph::compare, comm1=eval_g1_cl, comm2=eval_g2_cl)

  }

  list(
    result = l_df[, c("x", "y", "block")],
    method = ann,
    metrics = if (is.null(true_blocks)) NULL else eval_metrics,
    colnames = colnames_xy
   )
}
