#'
#' @importFrom mlpack lsh
#' @importFrom mlpack knn
#' @importFrom data.table data.table
#'
#' @title An internal function to use the LSH and KD-tree algorithm via the \link[mlpack]{mlpack} package.
#' @author Maciej Beręsewicz
#'
#' @param x deduplication or reference data,
#' @param y query data,
#' @param algo which algorithm should be used: \code{lsh} or \code{kd},
#' @param k number of neighbours to return,
#' @param verbose if TRUE, log messages to the console,
#' @param seed seed for the pseudo-random numbers algorithm,
#' @param path path to write the index,
#' @param control controls for the \code{lsh} or \code{kd} algorithms.
#'
#' @description
#' See details of \link[mlpack]{lsh} and \link[mlpack]{knn}.
#'
#' @keywords internal
method_mlpack <- function(x,
                          y,
                          algo = c("lsh", "kd"),
                          k,
                          verbose,
                          seed,
                          path,
                          control) {

  x <- as.matrix(x)
  y <- as.matrix(y)
  search_k <- max(1L, min(nrow(x), max(control$k_search, k)))
  effective_k <- min(k, search_k)

  result <- switch(algo,
                   "lsh" = mlpack::lsh(k = search_k,
                                       query = y,
                                       reference = x,
                                       verbose = verbose,
                                       seed = seed,
                                       bucket_size = control$lsh$bucket_size,
                                       hash_width = control$lsh$hash_width,
                                       num_probes = control$lsh$num_probes,
                                       projections = control$lsh$projections,
                                       tables = control$lsh$tables),
                   "kd" = mlpack::knn(k = search_k,
                                       query = y,
                                       reference = x,
                                       verbose = verbose,
                                       seed = seed,
                                       algorithm = control$kd$algorithm,
                                       leaf_size = control$kd$leaf_size,
                                       tree_type = control$kd$tree_type,
                                       eps = control$kd$epsilon,
                                       rho = control$kd$rho,
                                       tau = control$kd$tau,
                                       random_basis = control$kd$random_basis))


  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = result$neighbors[, effective_k] + 1,
                                 dist = result$distances[, effective_k])
  l_df
}



