#' Imports
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom RcppAnnoy AnnoyEuclidean
#' @importFrom RcppAnnoy AnnoyHamming
#' @importFrom RcppAnnoy AnnoyManhattan
#' @importFrom methods new
#' @importFrom data.table data.table
#'
#' @title An internal function to use Annoy algorithm via RcppAnnoy.
#' @author Maciej Beręsewicz
#'
#' @param x Deduplication or reference data.
#' @param y Query data.
#' @param k Number of neighbors to return.
#' @param distance distance metric
#' @param verbose If TRUE, log messages to the console.
#' @param seed seed for the pseudo-random numbers algorithm.
#' @param control controls for  \code{lsh} or \code{kd}.
#'
#' @description
#' See details of the [RcppAnnoy] package.
#'
#'

method_annoy <- function(x,
                         y,
                         k,
                         distance,
                         verbose,
                         seed,
                         control) {


  ncols <- ncol(x)

  l_ind <- switch(distance,
                  "euclidean" = methods::new(RcppAnnoy::AnnoyManhattan, ncols),
                  "manhatan"  = methods::new(RcppAnnoy::AnnoyManhattan, ncols),
                  "hamming"   = methods::new(RcppAnnoy::AnnoyHamming,   ncols),
                  "angular"   = methods::new(RcppAnnoy::AnnoyAngular,   ncols)
  )

  l_ind$setSeed(seed)

  if (verbose) l_ind$setVerbose(1)

  ## index
  for (i in 1:nrow(x)) l_ind$addItem(i - 1, x[i,])
  l_ind$build(control$annoy$n_trees)
  l_ind_nns <- numeric(length = nrow(y))

  ## query
  for (i in 1:nrow(y)) l_ind_nns[i] <- l_ind$getNNsByVector(y[i, ], k)[k]

  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = l_ind_nns + 1)
  l_df
}
