#' Imports
#' @import data.table
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom data.table setDT
#' @importFrom data.table merge.data.table
#' @importFrom data.table :=
#' @importFrom Matrix sparseMatrix
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
#' @param text input text data
#' @param distance distance metric for \code{RcppHNSW}
#' @param M see \code{RcppHNSW}
#' @param ef see \code{RcppHNSW}
#' @param verbose see \code{RcppHNSW}
#' @param progress see \code{RcppHNSW}
#' @param n_threads  see \code{RcppHNSW}
#'
#' @returns Returns vector with cluster indicators
#'
#' @export
blocking <- function(text,
                     distance = "cosine",
                     M = 25,
                     ef = 200,
                     verbose = T,
                     progress = "bar",
                     n_threads = 4) {

  l_tokens <- text2vec::itoken_parallel(iterable = text,
                                        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = 2L),
                                        n_chunks = 10,
                                        progressbar = T)

  l_voc <- text2vec::create_vocabulary(l_tokens)
  l_vec <- text2vec::vocab_vectorizer(l_voc)
  l_dtm <- text2vec::create_dtm(l_tokens, l_vec)

  l_dtm <- base::as.matrix(l_dtm)

  l_ind <- RcppHNSW::hnsw_build(X = l_dtm,
                                distance = distance,
                                M = M,
                                ef = ef,
                                verbose = verbose,
                                progress = progress,
                                n_threads = n_threads)

  l_1nn <- RcppHNSW::hnsw_search(X = l_dtm, ann = l_ind,
                                 k = 2, ef = ef, verbose = verbose, progress = progress, n_threads = n_threads)

  l_df <- data.table::data.table(l_1nn$idx)
  l_df[, id := 1:NROW(text)]
  l_df[, selected:= base::ifelse(X1 == id, X2, X1)]
  l_df_to_graph <- data.table::merge.data.table(x = l_df[, list(id, selected)],
                                                y = l_df[, list(id, selected)],
                                                by = "selected",
                                                allow.cartesian = TRUE)

  l_m_sp <- Matrix::sparseMatrix(i = l_df_to_graph$id.x, j = l_df_to_graph$id.y, x = 1)

  l_gr <- igraph::graph_from_adjacency_matrix(l_m_sp, mode = "undirected")
  l_clust <- igraph::components(l_gr, "weak")

  l_clust$membership
}
