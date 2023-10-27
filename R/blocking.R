#' Imports
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
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
#' @param ann algorithm to be used for searching for ann,
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
#'          control_ann = controls_ann(hsnw_M = 5, hnsw_ef = 10))
#'
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = FALSE,
                     ann = c("hnsw", "lsh"),
                     verbose = T,
                     progress = "bar",
                     n_threads = 1,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

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
                                  M = control_ann$hsnw_M,
                                  ef = control_ann$hnsw_ef,
                                  verbose = verbose,
                                  progress = progress,
                                  n_threads = n_threads)

    l_1nn <- RcppHNSW::hnsw_search(X = l_dtm,
                                   ann = l_ind,
                                   k = control_ann$k,
                                   ef = control_ann$hnsw_ef,
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
                                seed = 0)
    l_df <- base::as.data.frame(l_lhs_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }


  l_gr <- igraph::graph_from_data_frame(l_df[, c(3,2)], directed = F)
  l_clust <- igraph::components(l_gr, "weak")

  l_clust$membership
}
