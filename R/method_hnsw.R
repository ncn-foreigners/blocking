#'
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#'
#' @title An internal function to use hnsw algorithm via RcppHNSW.
#' @author Maciej Beręsewicz
#'
#' @param x Deduplication or reference data.
#' @param y Query data.
#' @param deduplication deduplication
#' @param k Number of neighbors to return.
#' @param distance 	Type of distance to calculate.
#' @param M Controls the number of bi-directional links created for each element during index construction.
#' @param ef_c Size of the dynamic list used during construction.
#' @param ef_s Size of the dynamic list used during search.
#' @param verbose If TRUE, log messages to the console.
#' @param n_threads Maximum number of threads to use.
#' @param grain_size Minimum amount of work to do (rows in X to add) per thread.
#'
#' @description
#' See details of [RcppHNSW::hnsw_build] and [RcppHNSW::hnsw_search]
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
                        n_threads,
                        grain_size) {
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
