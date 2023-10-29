#' Imports
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom RcppAnnoy AnnoyEuclidean
#' @importFrom RcppAnnoy AnnoyHamming
#' @importFrom RcppAnnoy AnnoyManhattan
#' @importFrom mlpack lsh
#' @importFrom mlpack knn
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
#' @importFrom methods new
#'
#'
#' @title Main function for blocking records given text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Function that creates shingles (strings with 2 characters), applies approximate nearest neibhour search using
#' \code{RcppHNSW} and creates blocks using \code{igraph}
#'
#' @param x input text or matrix data,
#' @param y input text or matrix data (default NULL),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("hnsw", "lsh", "annoy", "kd")},
#' default \code{"hnsw"}),
#' @param distance distance metric (default \code{cosine}),
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ann algorithms),
#' @param seed seed for the algorithms,
#' @param n_threads number of threads used for the ann,
#' @param control_txt list of controls for text data,
#' @param control_ann list of controls for ann algorithms.
#'
#' @returns Returns list with cluster indicators for x, y, ann method and colnames of the dtm
#'
#' @examples
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))
#'
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     ann = c("hnsw", "lsh", "annoy", "kd"),
                     distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
                     verbose = c(0, 1, 2),
                     n_threads = 1,
                     seed = 2023,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  ## defaults
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "hnsw"
  if (missing(distance)) distance <- "cosine"

  if (verbose %in% 1:2) cat("===== creating tokens =====\n")

  k <- 2L
  x_rows <- 1:NROW(x)

  ## now I only concatanate vectors but it is not efficient if two large datasets are present
  ## I should create two separate dtm matrices and then use intersect of names
  if (!is.null(y)) {
    y_rows <- (NROW(x)+1):(NROW(x) + NROW(y))
    x <- c(x, y)
  }

  l_tokens <- text2vec::itoken_parallel(
    iterable = x,
    tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
    n_chunks = control_txt$n_chunks,
    progressbar = verbose)

  l_voc <- text2vec::create_vocabulary(l_tokens)
  l_vec <- text2vec::vocab_vectorizer(l_voc)
  l_dtm <- text2vec::create_dtm(l_tokens, l_vec)

  l_dtm <- base::as.matrix(l_dtm) ## unfortunately we need to convert to dense matrix

  if (verbose %in% 1:2) cat(sprintf("===== starting search (%s, %d, %d) =====\n", ann, nrow(l_dtm), ncol(l_dtm)))

  ## switch with separate functions for each package?
  if (ann == "hnsw") {
    if (verbose == 2) verbose <- TRUE
    l_df <- method_hnsw(x = l_dtm,
                        y = NULL,
                        k = k,
                        distance = distance,
                        verbose = verbose,
                        n_threads = n_threads,
                        M = control_ann$hnsw$M,
                        ef_c = control_ann$hnsw$ef_c,
                        ef_s = control_ann$hnsw$ef_s)
  }

  if (ann == "lsh") {
    ## parameters should be provided in the controls
    if (verbose == 2) verbose <- TRUE

    l_lhs_result <- mlpack::lsh(k = k,
                                query = l_dtm,
                                reference = l_dtm,
                                verbose = verbose,
                                seed = seed,
                                bucket_size = control_ann$lsh$bucket_size,
                                hash_width = control_ann$lsh$hash_width,
                                num_probes = control_ann$lsh$num_probes,
                                projections = control_ann$lsh$projections,
                                tables = control_ann$lsh$tables)

    l_df <- base::as.data.frame(l_lhs_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }

  if (ann == "kd") {

    if (verbose == 2) verbose <- TRUE
    l_knn_result <- mlpack::knn(k = k,
                                query = l_dtm,
                                reference = l_dtm,
                                verbose = verbose,
                                seed = seed,
                                algorithm = control_ann$kd$algorithm,
                                leaf_size = control_ann$kd$leaf_size,
                                tree_type = control_ann$kd$tree_type,
                                eps = control_ann$kd$epsilon,
                                rho = control_ann$kd$rho,
                                tau = control_ann$kd$tau,
                                random_basis = control_ann$kd$random_basis)

    l_df <- base::as.data.frame(l_knn_result$neighbors) + 1
    l_df$id <- 1:NROW(l_df)
  }

  if (ann == "annoy") {

    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                distance %in% c("euclidean", "manhatan", "hamming", "angular"))

    l_ind <- switch(distance,
                    "euclidean" = methods::new(RcppAnnoy::AnnoyManhattan, ncol(l_dtm)),
                    "manhatan"  = methods::new(RcppAnnoy::AnnoyManhattan, ncol(l_dtm)),
                    "hamming"   = methods::new(RcppAnnoy::AnnoyHamming,   ncol(l_dtm)),
                    "angular"   = methods::new(RcppAnnoy::AnnoyAngular,   ncol(l_dtm))
                    )

    l_ind$setSeed(seed)

    if (verbose == 2) l_ind$setVerbose(1)

    for (i in 1:nrow(l_dtm)) l_ind$addItem(i - 1, l_dtm[i,])

    l_ind$build(control_ann$annoy$n_trees)
    l_ind_nns <- matrix(0, ncol=2, nrow = nrow(l_dtm))

    ## can be parallized
    for (i in 1:nrow(l_dtm)) l_ind_nns[i, ] <- l_ind$getNNsByVector(l_dtm[i,], 2)

    l_df <- base::as.data.frame(l_ind_nns + 1)
    l_df$id <- 1:NROW(l_ind_nns)
  }

  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  l_gr <- igraph::graph_from_data_frame(l_df[, c(3,2)], directed = F)
  l_clust <- igraph::components(l_gr, "weak")$membership

  list(
    x = unname(l_clust[x_rows]),
    y = if (!is.null(y)) unname(l_clust[y_rows]) else NULL,
    method = ann,
    colnames = colnames(l_dtm)
   )
}
