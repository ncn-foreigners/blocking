#'
#' @importFrom RcppHNSW hnsw_build
#' @importFrom RcppHNSW hnsw_search
#' @importFrom RcppHNSW HnswL2
#' @importFrom RcppHNSW HnswCosine
#' @importFrom RcppHNSW HnswIp
#' @importFrom data.table data.table
#' @importFrom methods new
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
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
#' @param path path to write the index.
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
                        path,
                        control) {

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

    ## add items from a sparse matrix in a batches
    if (verbose) {
      pb <- utils::txtProgressBar(style = 3)
    }
    starts <- seq(1, nrow(x), 1000) ## by 1000 batches

    for (i in 1:NROW(starts)) {
      ## check if last element is used
      l_ind$addItems(as.matrix(x[starts[i]:(starts[i]+999),]))
      if (exists("pb")) utils::setTxtProgressBar(pb,i)
    }
    if (exists("pb")) close(pb)

    ## query based on sparse data in batches
    l_ind$setEf(control$hnsw$ef_s)

    ## this should be changed to loop
    ## add items from a sparse matrix in a batches

    # if (verbose) {
    #   pb <- utils::txtProgressBar(style = 3)
    # }
    starts <- seq(1, nrow(x), 1000) ## by 1000 batches

    l_1nn <- list()
    l_1nn_m <- list()

    for (i in 1:NROW(starts)) {
      ## check if last element is used
      l_1nn_m[[i]] <- l_ind$getAllNNsList(as.matrix(y[starts[i]:(starts[i]+999),]), k, TRUE)$item
      #if (exists("pb")) utils::setTxtProgressBar(pb,i)
    }

    #if (exists("pb")) close(pb)

    l_1nn$idx <- do.call('rbind',l_1nn_m)

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
                                   k = k,
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
