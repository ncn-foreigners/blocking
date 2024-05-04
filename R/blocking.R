#' Imports
#' @importFrom text2vec itoken
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph make_clusters
#' @importFrom igraph compare
#' @importFrom utils combn
#'
#'
#' @title Block records based on text data.
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Function creates shingles (strings with 2 characters, default), applies approximate nearest neighbour (ANN) algorithms via the [rnndescent], RcppHNSW, [RcppAnnoy] and [mlpack] packages,
#' and creates blocks using graphs via [igraph].
#'
#' @param x reference data (a character vector or a matrix),
#' @param y query data (a character vector or a matrix), if not provided NULL by default and thus deduplication is performed,
#' @param deduplication whether deduplication should be applied (default TRUE as y is set to NULL),
#' @param on variables for ANN search (currently not supported),
#' @param on_blocking variables for blocking records before ANN search (currently not supported),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("nnd", "hnsw", "annoy", "lsh", "kd")}, default \code{"nnd"} which corresponds to nearest neighbour descent method),
#' @param distance distance metric (default \code{cosine}, more options are possible see details),
#' @param ann_write writing an index to file. Two files will be created: 1) an index, 2) and text file with column names,
#' @param ann_colnames file with column names if \code{x} or \code{y} are indices saved on the disk (currently not supported),
#' @param true_blocks matrix with true blocks to calculate evaluation metrics (standard metrics based on confusion matrix as well as all metrics from [igraph::compare()] are returned).
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ANN algorithm verbose used),
#' @param graph whether a graph should be returned (default FALSE),
#' @param seed seed for the algorithms (for reproducibility),
#' @param n_threads number of threads used for the ANN algorithms and adding data for index and query,
#' @param control_txt list of controls for text data (passed only to [text2vec::itoken_parallel] or [text2vec::itoken]),
#' @param control_ann list of controls for the ANN algorithms.
#'
#' @returns Returns a list with containing:\cr
#' \itemize{
#' \item{\code{result} -- \code{data.table} with indices (rows) of x, y, block and distance between points}
#' \item{\code{method} -- name of the ANN algorithm used,}
#' \item{\code{metrics} -- metrics for quality assessment, if \code{true_blocks} is provided,}
#' \item{\code{colnames} -- variable names (colnames) used for search,}
#' \item{\code{graph} -- \code{igraph} class object.}
#' }
#'
#' @examples
#'
#' ## an example using RcppHNSW
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))
#'
#' result
#'
#' ## an example using mlpack::lsh
#'
#' result_lsh <- blocking(x = df_example$txt,
#'                        ann = "lsh")
#'
#' result_lsh
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = TRUE,
                     on = NULL,
                     on_blocking = NULL,
                     ann = c("nnd", "hnsw", "annoy", "lsh", "kd"),
                     distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
                     ann_write = NULL,
                     ann_colnames = NULL,
                     true_blocks = NULL,
                     verbose = c(0, 1, 2),
                     graph = FALSE,
                     seed = 2023,
                     n_threads = 1,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  ## defaults
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "nnd"
  if (missing(distance)) distance <- switch(ann,
                                            "nnd" = "cosine",
                                            "hnsw" = "cosine",
                                            "annoy" = "angular",
                                            "lsh" = NULL,
                                            "kd" = NULL)

  stopifnot("Only character, dense or sparse (dgCMatrix) matrix x is supported" =
              is.character(x) | is.matrix(x) | inherits(x, "Matrix"))

  ## assuming rows (for nnd)
  stopifnot("Minimum 3 cases required for x" = NROW(x) > 2)

  if (!is.null(y)) {
    stopifnot("Minimum 3 cases required for y" = NROW(y) > 2)
  }


  if (!is.null(ann_write)) {
    stopifnot("Path provided in the `ann_write` is incorrect" = file.exists(ann_write) )
  }


  if (ann == "hnsw") {
    stopifnot("Distance for HNSW should be `l2, euclidean, cosine, ip`" =
                distance %in% c("l2", "euclidean", "cosine", "ip"))
  }

  if (ann == "annoy") {
    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                distance %in% c("euclidean", "manhatan", "hamming", "angular"))
  }



  if (!is.null(true_blocks)) {
    if (is.data.frame(true_blocks)) {
      stopifnot("`true blocks` should be a data.frame with columns: x, y, block" =
                  !is.null(true_blocks) &
                  is.data.frame(true_blocks) &
                  length(colnames(true_blocks)) == 3,
                all(colnames(true_blocks) == c("x", "y", "block")))
    }
    if (is.vector(true_blocks)) {
      stopifnot("`true blocks` should be a vector with elements equal to nrow(x)" =
                  NROW(true_blocks) == NROW(x))
    }
  }


  if (!is.null(y)) {
    deduplication <- FALSE
    y_default <- FALSE
    k <- 1L
  } else {
    y_default <- y
    y <- x
    k <- 2L
  }

  ## add verification if x and y is a sparse matrix
  if (is.matrix(x) | inherits(x, "Matrix")) {
    x_dtm <- x
    y_dtm <- y
  } else {

    if (verbose %in% 1:2) cat("===== creating tokens =====\n")

    ## tokens for x
    if (.Platform$OS.type == "unix") {
      x_tokens <- text2vec::itoken_parallel(
        iterable = x,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                        n = control_txt$n_shingles,
                                                                        lowercase = control_txt$lowercase,
                                                                        strip_non_alphanum = control_txt$strip_non_alphanum),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
    } else {
      x_tokens <- text2vec::itoken(
        iterable = x,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                        n = control_txt$n_shingles,
                                                                        lowercase = control_txt$lowercase,
                                                                        strip_non_alphanum = control_txt$strip_non_alphanum),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
    }

    x_voc <- text2vec::create_vocabulary(x_tokens)
    x_vec <- text2vec::vocab_vectorizer(x_voc)
    x_dtm <- text2vec::create_dtm(x_tokens, x_vec)

    if (is.null(y_default)) {
      y_dtm <- x_dtm
    } else {
      if (.Platform$OS.type == "unix") {
      y_tokens <- text2vec::itoken_parallel(
        iterable = y,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                        n = control_txt$n_shingles,
                                                                        lowercase = control_txt$lowercase,
                                                                        strip_non_alphanum = control_txt$strip_non_alphanum),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
      } else {
        y_tokens <- text2vec::itoken(
          iterable = y,
          tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                          n = control_txt$n_shingles,
                                                                          lowercase = control_txt$lowercase,
                                                                          strip_non_alphanum = control_txt$strip_non_alphanum),
          n_chunks = control_txt$n_chunks,
          progressbar = verbose)
      }
      y_voc <- text2vec::create_vocabulary(y_tokens)
      y_vec <- text2vec::vocab_vectorizer(y_voc)
      y_dtm <- text2vec::create_dtm(y_tokens, y_vec)

    }
  }

  colnames_xy <- intersect(colnames(x_dtm), colnames(y_dtm))

  if (verbose %in% 1:2) {
    cat(sprintf("===== starting search (%s, x, y: %d, %d, t: %d) =====\n",
                ann, nrow(x_dtm), nrow(y_dtm), length(colnames_xy)))
  }

  x_df <- switch(ann,
                 "nnd" = method_nnd(x = x_dtm[, colnames_xy],
                                    y = y_dtm[, colnames_xy],
                                    k = k,
                                    distance = distance,
                                    verbose = if (verbose == 2) TRUE else FALSE,
                                    n_threads = n_threads,
                                    control = control_ann),
                 "hnsw" = method_hnsw(x = x_dtm[, colnames_xy],
                                      y = y_dtm[, colnames_xy],
                                      k = k,
                                      distance = distance,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      n_threads = n_threads,
                                      path = ann_write,
                                      control = control_ann),
                 "lsh" = method_mlpack(x = x_dtm[, colnames_xy],
                                       y = y_dtm[, colnames_xy],
                                       algo = "lsh",
                                       k = k,
                                       verbose = if (verbose == 2) TRUE else FALSE,
                                       seed = seed,
                                       path = ann_write,
                                       control = control_ann),
                 "kd" = method_mlpack(x = x_dtm[, colnames_xy],
                                      y = y_dtm[, colnames_xy],
                                      algo = "kd",
                                      k = k,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      seed = seed,
                                      path = ann_write,
                                      control = control_ann),
                 "annoy" = method_annoy(x = x_dtm[, colnames_xy],
                                        y = y_dtm[, colnames_xy],
                                        k = k,
                                        distance  = distance,
                                        verbose = if (verbose == 2) TRUE else FALSE,
                                        seed = seed,
                                        path = ann_write,
                                        control = control_ann))


  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  ## remove duplicated pairs

  if (deduplication) x_df <- x_df[y > x]

  if (deduplication) {
    x_df[, `:=`("query_g", paste0("q", y))]
    x_df[, `:=`("index_g", paste0("q", x))]
  } else {
    x_df[, `:=`("query_g", paste0("q", y))]
    x_df[, `:=`("index_g", paste0("i", x))]
  }

  x_gr <- igraph::graph_from_data_frame(x_df[, c("query_g", "index_g")], directed = F)
  x_block <- igraph::components(x_gr, "weak")$membership

  x_df[, `:=`(block, x_block[names(x_block) %in% x_df$query_g])]

  ## if true are given
  if (!is.null(true_blocks)) {

    setDT(true_blocks) ## move it somewhere else

    pairs_to_eval <- x_df[y %in% true_blocks$y, c("x", "y", "block")]
    pairs_to_eval[true_blocks, on = c("x", "y"), both := TRUE]
    pairs_to_eval[is.na(both), both := FALSE]

    true_blocks[pairs_to_eval, on = c("x", "y"), both := TRUE]
    true_blocks[is.na(both), both := FALSE]
    true_blocks[, block:=block+max(pairs_to_eval$block)]

    pairs_to_eval <- rbind(pairs_to_eval, true_blocks[both == FALSE, .(x,y,block)], fill = TRUE)

    if (!deduplication) {

      pairs_to_eval[, x2:=x+max(y)]
      pairs_to_eval_long <- melt(pairs_to_eval[, .(y, x2, block, both)], id.vars = c("block", "both"))
      pairs_to_eval_long[!is.na(both), block_id := .GRP, block]
      block_id_max <- max(pairs_to_eval_long$block_id, na.rm = T)
      pairs_to_eval_long[is.na(both), block_id:=block_id_max + rleid(block)]
      pairs_to_eval_long[both == TRUE | is.na(both), true_id := .GRP, block]
      true_id_max <- max(pairs_to_eval_long$true_id, na.rm = T)
      pairs_to_eval_long[both==FALSE, true_id := true_id_max+rleid(block)]

    } else {

      pairs_to_eval_long <- melt(pairs_to_eval[, .(y, x, block, both)], id.vars = c("block", "both"))
      pairs_to_eval_long[!is.na(both), block_id := .GRP, block]
      block_id_max <- max(pairs_to_eval_long$block_id, na.rm = T)
      pairs_to_eval_long[is.na(both), block_id:=block_id_max + rleid(block)]
      pairs_to_eval_long[both == TRUE | is.na(both), true_id := .GRP, block]
      true_id_max <- max(pairs_to_eval_long$true_id, na.rm = T)
      pairs_to_eval_long[both==FALSE, true_id := true_id_max+rleid(block)]

    }

    #consider using RcppAlgos::comboGeneral(nrow(pairs_to_eval_long), 2,  nThreads=n_threads)
    candidate_pairs <- utils::combn(nrow(pairs_to_eval_long), 2)

    same_block <- pairs_to_eval_long$block_id[candidate_pairs[1, ]] == pairs_to_eval_long$block_id[candidate_pairs[2, ]]
    same_truth <- pairs_to_eval_long$true_id[candidate_pairs[1, ]] == pairs_to_eval_long$true_id[candidate_pairs[2, ]]

    confusion <- table(same_block, same_truth)

    fp <- confusion[2, 1]
    fn <- confusion[1, 2]
    tp <- confusion[2, 2]
    tn <- confusion[1, 1]
    recall <- tp/(fn + tp)

    eval_metrics <- c(recall = tp / (fn + tp), precision = tp / (tp + fp),
                      fpr = fp / (fp + tn), fnr = fn / (fn + tp),
                      accuracy = (tp + tn) / (tp + tn + fn + fp),
                      specificity = tn / (tn + fp))

  }

  setorderv(x_df, c("x", "y", "block"))

  structure(
    list(
      result = x_df[, c("x", "y", "block", "dist")],
      method = ann,
      deduplication = deduplication,
      metrics = if (is.null(true_blocks)) NULL else eval_metrics,
      colnames = colnames_xy,
      graph = if (graph) {
        igraph::graph_from_data_frame(x_df[, c("x", "y")], directed = F)
        } else NULL
   ),
   class = "blocking"
  )
}
