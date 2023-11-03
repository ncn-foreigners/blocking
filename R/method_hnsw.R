#'
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom data.table data.table
#'
#' @title An internal function to use hnsw algorithm via RcppHNSW.
#' @author Maciej Beręsewicz
#'
#' @param x Deduplication or reference data.
#' @param y Query data.
#' @param k Number of neighbors to return.
#' @param distance 	Type of distance to calculate.
#' @param verbose If TRUE, log messages to the console.
#' @param n_threads Maximum number of threads to use.
#' @param control Controls for the HNSW algorithm
#'
#' @description
#' See details of [RcppHNSW::hnsw_build] and [RcppHNSW::hnsw_search]
#'
#'
method_hnsw <- function(x,
                        y,
                        k,
                        distance,
                        verbose,
                        n_threads,
                        control) {
  ## index
  l_ind <- RcppHNSW::hnsw_build(X = x,
                                distance = distance,
                                verbose = verbose,
                                n_threads = n_threads,
                                M = control$hnsw$M,
                                ef = control$hnsw$ef_c)
  ## query
  l_1nn <- RcppHNSW::hnsw_search(X = y,
                                 ann = l_ind,
                                 k = k,
                                 ef = control$hnsw$ef_s,
                                 verbose = verbose,
                                 n_threads = n_threads)

  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = l_1nn$idx[, k])

  l_df
}
