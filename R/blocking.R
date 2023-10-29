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
#' @importFrom RANN nn2
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
#' @param verbose whether progress should be provided,
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
#' blocking(x = df_example$txt, ann = "hnsw",
#'          control_ann = controls_ann(hnsw=list(M = 5, ef_b = 10, ef_s = 10)))
#'
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = FALSE,
                     ann = c("hnsw", "lsh", "annoy", "kd"),
                     verbose = T,
                     progress = "bar",
                     n_threads = 1,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  if (missing(ann)) ann <- "hnsw"

  l_tokens <- text2vec::itoken_parallel(
    iterable = x,
    tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
    n_chunks = control_txt$n_chunks,
    progressbar = verbose)

  l_voc <- text2vec::create_vocabulary(l_tokens)
  l_vec <- text2vec::vocab_vectorizer(l_voc)
  l_dtm <- text2vec::create_dtm(l_tokens, l_vec)

  l_dtm <- base::as.matrix(l_dtm) ## unfortunately we need to convert to dense matrix

  ## switch with separate functions for each package?
  if (ann == "hnsw") {
    l_ind <- RcppHNSW::hnsw_build(X = l_dtm,
                                  distance = control_ann$distance,
                                  M = control_ann$hnsw$M,
                                  ef = control_ann$hnsw$ef_s,
                                  verbose = verbose,
                                  progress = progress,
                                  n_threads = n_threads)

    l_1nn <- RcppHNSW::hnsw_search(X = l_dtm,
                                   ann = l_ind,
                                   k = control_ann$k,
                                   ef = control_ann$hnsw$ef_s,
                                   verbose = verbose,
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
                                verbose = verbose,
                                seed = control_ann$seed)
    l_df <- base::as.data.frame(l_lhs_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }
  if (ann == "annoy") {
    l_ind <- methods::new(RcppAnnoy::AnnoyAngular, ncol(l_dtm))
    l_ind$setSeed(control_ann$seed)
    l_ind$setVerbose(verbose)
    for (i in 1:nrow(l_dtm)) l_ind$addItem(i - 1, l_dtm[i,])
    l_ind$build(control_ann$annoy$n_trees)
    l_ind_nns <- matrix(0, ncol=2, nrow = nrow(l_dtm))
    for (i in 1:nrow(l_dtm)) l_ind_nns[i, ] <- l_ind$getNNsByVector(l_dtm[i,], 2)
    l_ind_nns <- base::as.data.frame(l_ind_nns + 1)
    l_ind_nns$id <- 1:NROW(l_ind_nns)
  }
  if (ann == "kd") {
    l_df <- base::as.data.frame(RANN::nn2(data = l_dtm,
                                          query = l_dtm,
                                          k = control_ann$k,
                                          treetype = "kd",
                                          eps = control_ann$kd$eps)$nn.idx)
    l_df$id <- 1:NROW(l_dtm)
  }
  l_gr <- igraph::graph_from_data_frame(l_df[, c(3,2)], directed = F)
  l_clust <- igraph::components(l_gr, "weak")

  l_clust$membership
}
