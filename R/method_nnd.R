#'
#' @importFrom rnndescent rnnd_build
#' @importFrom rnndescent rnnd_query
#' @importFrom data.table data.table
#'
#' @title An internal function to use the NN descent algorithm via the [rnndescent] package.
#' @author Maciej Beręsewicz
#'
#' @param x deduplication or reference data,
#' @param y query data,
#' @param k number of neighbours to return,
#' @param distance 	type of distance to calculate,
#' @param deduplication whether the deduplication is applied,
#' @param verbose if TRUE, log messages to the console,
#' @param n_threads maximum number of threads to use,
#' @param control controls for the NN descent algorithm.
#'
#' @description
#' See details of [rnndescent::rnnd_build] and [rnndescent::rnnd_query].
#'
#'
method_nnd <- function(x,
                       y,
                       k,
                       distance,
                       deduplication,
                       verbose,
                       n_threads,
                       control) {

  l_ind <- rnndescent::rnnd_build(data = x,
                                  k = if (nrow(x) < control$nnd$k_build) nrow(x)-1 else control$nnd$k_build,
                                  metric = distance,
                                  verbose = verbose,
                                  n_threads = n_threads,
                                  use_alt_metric = control$nnd$use_alt_metric,
                                  init = control$nnd$init,
                                  n_trees = control$nnd$n_trees,
                                  leaf_size = control$nnd$leaf_size,
                                  max_tree_depth = control$nnd$max_tree_depth,
                                  margin = control$nnd$margin,
                                  n_iters = control$nnd$n_iters,
                                  delta = control$nnd$delta,
                                  max_candidates = control$nnd$max_candidates,
                                  low_memory = control$nnd$low_memory,
                                  n_search_trees = control$nnd$n_search_trees,
                                  pruning_degree_multiplier = control$nnd$pruning_degree_multiplier,
                                  diversify_prob = control$nnd$diversify_prob,
                                  weight_by_degree = control$nnd$weight_by_degree,
                                  prune_reverse = control$nnd$prune_reverse,
                                  progress = control$nnd$progress,
                                  obs = control$nnd$obs)

  ## query k dependent on the study
  ## there is a problem when dataset is small

  if (deduplication == T) {
    k_nnd_query <- k
  } else if (nrow(x) < 10) {
    k_nnd_query <- k
  } else if (nrow(x) < control$k_search) {
    k_nnd_query <- nrow(x)
  } else {
    k_nnd_query <- control$k_search
  }

  l_1nn <- rnndescent::rnnd_query(index = l_ind,
                                  query = y,
                                  k = k_nnd_query,
                                  epsilon = 0.1,
                                  max_search_fraction = 1,
                                  init = NULL,
                                  verbose = verbose,
                                  n_threads = n_threads,
                                  obs = "R")

  # if (!is.null(path)) {
  #   if (grepl("(/|\\\\)$", path)) {
  #     path_ann <- paste0(path, "index.hnsw")
  #     path_ann_cols <- paste0(path, "index-colnames.txt")
  #   } else {
  #     path_ann <- paste0(path, "//index.hnsw")
  #     path_ann_cols <- paste0(path, "//index-colnames.txt")
  #   }
  #   if (verbose) {
  #       cat("Writing an index to `path`\n")
  #   }
  #   l_ind$save(path_ann)
  #   writeLines(colnames(x), path_ann_cols)
  # }

  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = l_1nn$idx[, k],
                                 dist = l_1nn$dist[,k])


  l_df
}
