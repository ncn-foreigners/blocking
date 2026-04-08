#'
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom RcppAnnoy AnnoyEuclidean
#' @importFrom RcppAnnoy AnnoyHamming
#' @importFrom RcppAnnoy AnnoyManhattan
#' @importFrom methods new
#' @importFrom data.table data.table
#'
#' @title An internal function to use Annoy algorithm via the \link[RcppAnnoy]{RcppAnnoy} package.
#' @author Maciej Beręsewicz
#'
#' @param x deduplication or reference data,
#' @param y query data,
#' @param k number of neighbours to return,
#' @param distance distance metric,
#' @param verbose if TRUE, log messages to the console,
#' @param seed seed for the pseudo-random numbers algorithm,
#' @param path path to write the index,
#' @param control controls for \code{new} or \code{build} methods for \link[RcppAnnoy]{RcppAnnoy}.
#'
#' @description
#' See details of the \link[RcppAnnoy]{RcppAnnoy} package.
#'
#' @keywords internal
method_annoy <- function(x,
                         y,
                         k,
                         distance,
                         verbose,
                         path,
                         seed,
                         control) {


  search_k <- max(1L, min(nrow(x), max(control$k_search, k)))
  effective_k <- min(k, search_k)

  ncols <- ncol(x)

  l_ind <- switch(distance,
                  "euclidean" = methods::new(RcppAnnoy::AnnoyEuclidean, ncols),
                  "manhatan"  = methods::new(RcppAnnoy::AnnoyManhattan, ncols),
                  "hamming"   = methods::new(RcppAnnoy::AnnoyHamming, ncols),
                  "angular"   = methods::new(RcppAnnoy::AnnoyAngular, ncols)
  )

  l_ind$setSeed(seed)

  if (control$annoy$build_on_disk) {
    temp_annoy <- tempfile(pattern="annoy", fileext="tree")
    cat("Building index on disk:", temp_annoy, "\n")
    l_ind$onDiskBuild(temp_annoy)

  }
  if (verbose) l_ind$setVerbose(1)

  ## index - this does not require dense matrix
  for (i in 1:nrow(x)) l_ind$addItem(i - 1, x[i,])
  l_ind$build(control$annoy$n_trees)
  l_ind_nns <- numeric(length = nrow(y))
  l_ind_dist <- numeric(length = nrow(y))

  ## query
  for (i in 1:nrow(y)) {
    annoy_res <- l_ind$getNNsByVectorList(y[i, ],
                                          search_k,
                                          -1,
                                          TRUE)
    l_ind_nns[i] <- annoy_res$item[effective_k]
    l_ind_dist[i] <- annoy_res$distance[effective_k]
  }

  if (!is.null(path)) {
    if (grepl("(/|\\\\)$", path)) {
      path_ann <- paste0(path, "index.annoy")
      path_ann_cols <- paste0(path, "index-colnames.txt")
    } else {
      path_ann <- paste0(path, "//index.annoy")
      path_ann_cols <- paste0(path, "//index-colnames.txt")
    }
    if (verbose) {
      cat("Writing an index to `path`\n")
    }
    l_ind$save(path_ann)
    writeLines(colnames(x), path_ann_cols)
  }

  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = l_ind_nns + 1,
                                 dist = l_ind_dist)
  l_df
}
