#'
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#'
#' @description
#' Tu bedzie opis
#'
#' @title Internal function of hnsw via RcppHNSW
#' @author Maciej Beręsewicz
#'
#' @param x x
#' @param y y
#' @param deduplication deduplication
#' @param k k
#' @param distance distance
#' @param M m
#' @param ef_c ef
#' @param ef_s ef
#' @param verbose ver
#' @param n_threads trh
#'
#'
method_hnsw <- function(x,
                        y,
                        deduplication,
                        k,
                        distance,
                        M,
                        ef_c,
                        ef_s,
                        verbose = 0,
                        n_threads) {
  ## index
  l_ind <- RcppHNSW::hnsw_build(X = x,
                                distance = distance,
                                M = M,
                                ef = ef_c,
                                verbose = verbose,
                                n_threads = n_threads)
  ## query
  l_1nn <- RcppHNSW::hnsw_search(X = y,
                                 ann = l_ind,
                                 k = k,
                                 ef = ef_s,
                                 verbose = verbose,
                                 n_threads = n_threads)

  l_df <- base::data.frame(y = 1:NROW(y),
                           x = l_1nn$idx[, k])

  l_df
}
