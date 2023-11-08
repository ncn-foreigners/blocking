#'
#' @importFrom mlpack lsh
#' @importFrom mlpack knn
#' @importFrom data.table data.table
#'
#' @title An internal function to use algorthms from the mlpack package.
#' @author Maciej Beręsewicz
#'
#' @param x Deduplication or reference data.
#' @param y Query data.
#' @param algo Which algorithm should be used. Possible: \code{lsh} or \code{kd}.
#' @param k Number of neighbors to return.
#' @param verbose If TRUE, log messages to the console.
#' @param seed seed for the pseudo-random numbers algorithm.
#' @param control controls for  \code{lsh} or \code{kd}.
#'
#' @description
#' See details of [mlpack::lsh] and [mlpack::knn]
#'
method_mlpack <- function(x,
                          y,
                          algo = c("lsh", "kd"),
                          k,
                          verbose,
                          seed,
                          control) {

  ## TODO: separate building an index from querying

  ## conversion from sparse to dense matrix
  ## this should be done with verification of the size
  ## calculate size based on 8*1e6/(2^20)
  ## source: https://stackoverflow.com/questions/45332767/how-the-object-size-in-r-are-calculated

  x <- as.matrix(x)
  y <- as.matrix(y)

  result <- switch(algo,
                   "lsh" = mlpack::lsh(k = k,
                                       query = y,
                                       reference = x,
                                       verbose = verbose,
                                       seed = seed,
                                       bucket_size = control$lsh$bucket_size,
                                       hash_width = control$lsh$hash_width,
                                       num_probes = control$lsh$num_probes,
                                       projections = control$lsh$projections,
                                       tables = control$lsh$tables),
                   "kd" = mlpack::knn(k = k,
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
                                 x = result$neighbors[, k] + 1)
  l_df
}




