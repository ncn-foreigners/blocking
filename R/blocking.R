#' Imports
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom RcppAnnoy AnnoyEuclidean
#' @importFrom RcppAnnoy AnnoyHamming
#' @importFrom RcppAnnoy AnnoyManhattan
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
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
#' @param deduplication generate pairs from only \code{x},
#' @param ann algorithm to be used for searching for ann (possible, \code{c("hnsw", "lsh", "annoy")}, default \code{"hnsw"}),
#' @param verbose whether progress should be provided (0 = none, 1 = main, 2 = ann algorithms),
#' @param progress how the progress should be presented,
#' @param n_threads number of threads used for the ann,
#' @param control_txt list of controls for text data,
#' @param control_ann list of controls for ann algorithms.
#'
#' @returns Returns vector with cluster indicators.
#'
#' @examples
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' blocking(x = df_example$txt,
#'          ann = "hnsw",
#'          control_ann = controls_ann(hnsw = list(M = 5, ef_b = 10, ef_s = 10)))
#'
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = TRUE,
                     ann = c("hnsw", "lsh", "annoy", "kd"),
                     verbose = 0,
                     progress = "bar",
                     n_threads = 1,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  if (missing(ann)) ann <- "hnsw"

  if (verbose == 1) cat("===== creating tokens =====\n")

  l_tokens <- text2vec::itoken_parallel(
    iterable = x,
    tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
    n_chunks = control_txt$n_chunks,
    progressbar = verbose)

  l_voc <- text2vec::create_vocabulary(l_tokens)
  l_vec <- text2vec::vocab_vectorizer(l_voc)
  l_dtm <- text2vec::create_dtm(l_tokens, l_vec)

  l_dtm <- base::as.matrix(l_dtm) ## unfortunately we need to convert to dense matrix

  if (verbose == 1) cat(sprintf("===== starting search (%s, %d, %d) =====\n",
                                ann, nrow(l_dtm), ncol(l_dtm)))

  ## switch with separate functions for each package?
  if (ann == "hnsw") {
    l_ind <- RcppHNSW::hnsw_build(X = l_dtm,
                                  distance = control_ann$distance,
                                  M = control_ann$hnsw$M,
                                  ef = control_ann$hnsw$ef_s,
                                  verbose = if (verbose == 2) TRUE,
                                  progress = progress,
                                  n_threads = n_threads)

    l_1nn <- RcppHNSW::hnsw_search(X = l_dtm,
                                   ann = l_ind,
                                   k = control_ann$k,
                                   ef = control_ann$hnsw$ef_s,
                                   verbose = if (verbose == 2) TRUE,
                                   progress = progress,
                                   n_threads = n_threads)

    l_df <- base::as.data.frame(l_1nn$idx)
    l_df$id <- 1:NROW(l_df)
  }

  if (ann == "lsh") {
    ## parameters should be provided in the controls
    l_lhs_result <- mlpack::lsh(bucket_size = 500,
                                hash_width = 10,
                                projections = 10,
                                tables = 30,
                                k = control_ann$k,
                                query = l_dtm,
                                reference = l_dtm,
                                verbose = if (verbose == 2) TRUE,
                                seed = control_ann$seed)
    l_df <- base::as.data.frame(l_lhs_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }

  if (ann == "kd") {
    ## should be removed for mlpack::knn instead
    l_knn_result <- mlpack::knn(k = control_ann$k,
                                leaf_size = 20,
                                query = l_dtm,
                                reference = l_dtm,
                                tree_type = "kd",
                                verbose = if (verbose == 2) TRUE,
                                seed = control_ann$seed,
                                eps = control_ann$kd$eps,
                                random_basis = FALSE)

    l_df <- base::as.data.frame(l_knn_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }

  if (ann == "annoy") {

    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                control_ann$distance %in% c("euclidean", "manhatan", "hamming", "angular"))

    l_ind <- switch(control_ann$distance,
                    "euclidean" = methods::new(RcppAnnoy::AnnoyManhattan, ncol(l_dtm)),
                    "manhatan"  = methods::new(RcppAnnoy::AnnoyManhattan, ncol(l_dtm)),
                    "hamming"   = methods::new(RcppAnnoy::AnnoyHamming, ncol(l_dtm)),
                    "angular"   = methods::new(RcppAnnoy::AnnoyAngular, ncol(l_dtm))
                    )

    l_ind$setSeed(control_ann$seed)
    l_ind$setVerbose(if (verbose == 2) TRUE)

    for (i in 1:nrow(l_dtm)) l_ind$addItem(i - 1, l_dtm[i,])

    l_ind$build(control_ann$annoy$n_trees)
    l_ind_nns <- matrix(0, ncol=2, nrow = nrow(l_dtm))

    for (i in 1:nrow(l_dtm)) l_ind_nns[i, ] <- l_ind$getNNsByVector(l_dtm[i,], 2)

    l_df <- base::as.data.frame(l_ind_nns + 1)
    l_df$id <- 1:NROW(l_ind_nns)
  }

  if (verbose) cat("===== creating graph =====\n")

  l_gr <- igraph::graph_from_data_frame(l_df[, c(3,2)], directed = F)
  l_clust <- igraph::components(l_gr, "weak")

  l_clust$membership
}
