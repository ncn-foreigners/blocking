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
                        y = NULL,
                        k,
                        distance,
                        M,
                        ef_c,
                        ef_s,
                        verbose = 0,
                        n_threads) {

  l_ind <- RcppHNSW::hnsw_build(X = x,
                                distance = distance,
                                M = M,
                                ef = ef_c,
                                verbose = verbose,
                                n_threads = n_threads)

  l_1nn <- RcppHNSW::hnsw_search(X = x,
                                 ann = l_ind,
                                 k = k,
                                 ef = ef_s,
                                 verbose = verbose,
                                 n_threads = n_threads)

  l_df <- base::as.data.frame(l_1nn$idx)
  l_df$id <- 1:NROW(l_df)
  l_df
}
