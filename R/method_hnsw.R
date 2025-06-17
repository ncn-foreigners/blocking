#'
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom RcppHNSW HnswL2
#' @importFrom RcppHNSW HnswCosine
#' @importFrom RcppHNSW HnswIp
#' @importFrom data.table data.table
#' @importFrom methods new
#'
#' @title An internal function to use HNSW algorithm via the RcppHNSW package.
#' @author Maciej Beręsewicz
#'
#' @param x deduplication or reference data,
#' @param y query data,
#' @param k number of neighbours to return,
#' @param distance 	type of distance to calculate,
#' @param verbose if TRUE, log messages to the console,
#' @param n_threads Maximum number of threads to use,
#' @param path path to write the index,
#' @param control controls for the HNSW algorithm.
#'
#' @description
#' See details of \link[RcppHNSW]{hnsw_build} and \link[RcppHNSW]{hnsw_search}.
#'
#' @keywords internal
method_hnsw <- function(x,
                        y,
                        k,
                        distance,
                        verbose,
                        n_threads,
                        path,
                        control,
                        seed) {

  set.seed(seed)

  ## depending whether x is an
  ## to avoid coping to marix
  if (control$sparse) {
    index <- switch(distance,
                    "l2" = RcppHNSW::HnswL2,
                    "euclidean" = RcppHNSW::HnswL2,
                    "cosine" = RcppHNSW::HnswCosine,
                    "ip" = RcppHNSW::HnswIp)


    l_ind <- methods::new(index, ncol(x), nrow(x), control$hnsw$M, control$hnsw$ef_c)
    l_ind$setNumThreads(n_threads)
    l_ind$setGrainSize(control$hnsw$grain_size)

    for (i in 1:nrow(x)) {
      l_ind$addItem(x[i,])
    }

    l_ind$setEf(control$hnsw$ef_s)

    ## this does not handle the control$k_search parameter
    l_1nn_m <- list()
    for (i in 1:nrow(y)) {
      l_1nn_m[[i]] <- l_ind$getNNsList(y[i,],
                                       k,
                                       TRUE)
    }

    l_1nn <- list(idx = do.call("rbind",lapply(l_1nn_m, "[[", "item")),
                  dist = do.call("rbind",lapply(l_1nn_m, "[[", "distance")))

  } else {
    x <- as.matrix(x)
    y <- as.matrix(y)

    l_ind <- RcppHNSW::hnsw_build(X = x,
                                  distance = distance,
                                  verbose = verbose,
                                  n_threads = n_threads,
                                  M = control$hnsw$M,
                                  ef = control$hnsw$ef_c)

    ## query
    l_1nn <- RcppHNSW::hnsw_search(X = y,
                                   ann = l_ind,
                                   k = if (nrow(x) < control$k_search) nrow(x) else control$k_search,
                                   ef = control$hnsw$ef_s,
                                   verbose = verbose,
                                   n_threads = n_threads)
  }


  if (!is.null(path)) {
    if (grepl("(/|\\\\)$", path)) {
      path_ann <- paste0(path, "index.hnsw")
      path_ann_cols <- paste0(path, "index-colnames.txt")
    } else {
      path_ann <- paste0(path, "//index.hnsw")
      path_ann_cols <- paste0(path, "//index-colnames.txt")
    }
    if (verbose) {
        cat("Writing an index to `path`\n")
    }
    l_ind$save(path_ann)
    writeLines(colnames(x), path_ann_cols)
  }

  l_df <- data.table::data.table(y = 1:NROW(y),
                                 x = l_1nn$idx[, k],
                                 dist = l_1nn$dist[,k])



  l_df
}
